/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_base_db::FilePosition;
use elp_syntax::AstNode;
use elp_syntax::ast::Expr;
use elp_syntax::ast::MapExpr;
use fxhash::FxHashMap;
use hir::AnyExpr;
use hir::DefMap;
use hir::InFile;
use hir::NameArity;
use hir::Semantic;
use hir::Spec;
use hir::SpecDef;
use hir::Strategy;
use hir::TypeAliasDef;
use hir::TypeExpr;
use hir::fold::Fold;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;
use crate::Kind;

pub(crate) fn add_completions(acc: &mut Vec<Completion>, args: &Ctx) -> DoneFlag {
    add_token_based_completions(acc, args)
}

fn add_token_based_completions(
    acc: &mut Vec<Completion>,
    Ctx {
        file_position,
        previous_tokens,
        sema,
        trigger,
        ..
    }: &Ctx,
) -> DoneFlag {
    use elp_syntax::SyntaxKind as K;
    let default = vec![];
    let previous_tokens: &[_] = previous_tokens.as_ref().unwrap_or(&default);
    match previous_tokens {
        // #~
        [.., (K::ANON_POUND, _)] if matches!(trigger, Some('#') | None) => {
            let def_map = sema.def_map(file_position.file_id);
            let mut types = FxHashMap::default();
            types_from_declarations(&mut types, &def_map);
            types_from_specs(&mut types, sema, &def_map);
            for (name, def) in types {
                let db = sema.db.upcast();
                if let Some(expr) = def.map_expr_for_completion(db) {
                    acc.push(completion(sema, &name, &def, &expr, ":=".to_string()));
                    acc.push(completion(sema, &name, &def, &expr, "=>".to_string()));
                }
            }
            false
        }
        _ => false,
    }
}

fn types_from_declarations(res: &mut FxHashMap<NameArity, TypeAliasDef>, def_map: &DefMap) {
    for (name_arity, alias) in def_map.get_types() {
        res.insert(name_arity.clone(), alias.clone());
    }
}

fn types_from_specs(
    res: &mut FxHashMap<NameArity, TypeAliasDef>,
    sema: &Semantic,
    def_map: &DefMap,
) {
    for (_name, function_def) in def_map.get_functions() {
        if let Some(spec) = &function_def.spec {
            types_from_spec(res, sema, spec);
        }
    }
    for spec in def_map.get_unowned_specs().values() {
        types_from_spec(res, sema, spec);
    }
}

fn types_from_spec(res: &mut FxHashMap<NameArity, TypeAliasDef>, sema: &Semantic, spec: &SpecDef) {
    let spec_id = InFile::new(spec.file.file_id, spec.spec_id);
    let spec_body = sema.db.spec_body(spec_id);
    Spec::fold(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        spec_id,
        (),
        &mut |_acc, ctx| {
            if let AnyExpr::TypeExpr(TypeExpr::Call { target, args }) = ctx.item {
                let arity = args.len();
                if let Some(alias_def) =
                    target.resolve_call(arity as u32, sema, spec.file.file_id, &spec_body.body)
                {
                    res.insert(alias_def.type_alias.name().clone(), alias_def);
                }
            }
        },
    );
}

fn position(sema: &Semantic, def: &TypeAliasDef) -> Option<FilePosition> {
    let offset = def
        .source(sema.db.upcast())
        .type_name()?
        .syntax()
        .text_range()
        .start();
    Some(FilePosition {
        file_id: def.file.file_id,
        offset,
    })
}
fn completion(
    sema: &Semantic,
    name: &NameArity,
    def: &TypeAliasDef,
    expr: &MapExpr,
    op: String,
) -> Completion {
    // Setting a position ensures a "resolve" request will be triggered when the user selects the completion.
    let preview = preview(expr, &op);
    let snippet = snippet(expr, &op);
    Completion {
        label: format!("{} {preview}", name.as_label()),
        kind: Kind::Map,
        contents: Contents::Snippet(snippet),
        position: position(sema, def),
        sort_text: None,
        deprecated: false,
        additional_edit: None,
    }
}

fn capitalize_first_char(word: String) -> Option<String> {
    let mut chars = word.chars();
    chars
        .next()
        .map(|char| char.to_uppercase().to_string() + chars.as_str())
}

fn preview(def: &MapExpr, op: &str) -> String {
    let field = def.fields().next();
    match field {
        Some(field) => {
            if let Some(key) = field.key() {
                let value = capitalize_first_char(key.syntax().text().to_string())
                    .unwrap_or("...".to_string());
                if def.fields().count() == 1 {
                    format!("#{{{key} {op} {value}}}")
                } else {
                    format!("#{{{key} {op} {value}, ... }}")
                }
            } else {
                "#{{ ... }}".to_string()
            }
        }
        None => "#{{}}".to_string(),
    }
}

fn snippet(def: &MapExpr, op: &str) -> String {
    let mut snippets = Vec::new();
    _ = def.fields().fold(1, |idx, field| {
        if let Some(key) = field.key() {
            snippets.push(format_field(idx, key, op));
        }
        idx + 1
    });
    format!("{{{}}}", snippets.join(", "))
}

fn format_field(idx: i32, key: Expr, op: &str) -> String {
    let value = capitalize_first_char(key.syntax().text().to_string());
    let value = value.unwrap_or("Value".to_string());
    format!("{key} {op} ${{{idx}:{value}}}")
}

#[cfg(test)]
mod test {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::Kind;
    use crate::tests::get_completions;
    use crate::tests::render_completions;

    fn check(code: &str, trigger_character: Option<char>, expect: Expect) {
        let completions = get_completions(code, trigger_character)
            .into_iter()
            .filter(|c| c.kind != Kind::Keyword)
            .collect();
        let actual = &render_completions(completions);
        expect.assert_eq(actual);
    }

    #[test]
    fn test_local_type() {
        check(
            r#"
         //- expect_parse_errors
         -module(test_local_type).
         -type my_map() :: #{field1 := integer(), field2 => boolean()}.
         foo(X) -> #~
         "#,
            None,
            expect![[r#"
                {label:my_map/0 #{field1 := Field1, ... }, kind:Map, contents:Snippet("{field1 := ${1:Field1}, field2 := ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 32 })}
                {label:my_map/0 #{field1 => Field1, ... }, kind:Map, contents:Snippet("{field1 => ${1:Field1}, field2 => ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 32 })}"#]],
        );
    }

    #[test]
    fn test_included_type() {
        check(
            r#"
         //- expect_parse_errors
         //- /include/test_included_type.hrl include_path:/include
         -type my_included_map() :: #{field1 := integer(), field2 => integer()}.
         //- /src/test_included_type.erl
         -module(test_included_type).
         -include("test_included_type.hrl").
         -type my_local_map() :: #{field3 := integer(), field4 => boolean()}.
         foo(X) -> X#~
         "#,
            None,
            expect![[r#"
                {label:my_included_map/0 #{field1 := Field1, ... }, kind:Map, contents:Snippet("{field1 := ${1:Field1}, field2 := ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 6 })}
                {label:my_included_map/0 #{field1 => Field1, ... }, kind:Map, contents:Snippet("{field1 => ${1:Field1}, field2 => ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 6 })}
                {label:my_local_map/0 #{field3 := Field3, ... }, kind:Map, contents:Snippet("{field3 := ${1:Field3}, field4 := ${2:Field4}}"), position:Some(FilePosition { file_id: FileId(1), offset: 71 })}
                {label:my_local_map/0 #{field3 => Field3, ... }, kind:Map, contents:Snippet("{field3 => ${1:Field3}, field4 => ${2:Field4}}"), position:Some(FilePosition { file_id: FileId(1), offset: 71 })}"#]],
        );
    }

    #[test]
    fn test_empty_map_type() {
        check(
            r#"
         //- expect_parse_errors
         -module(test_empty_map_type).
         -type my_map() :: #{}.
         foo(X) -> #~
         "#,
            None,
            expect![[r#"
                {label:my_map/0 #{{}}, kind:Map, contents:Snippet("{}"), position:Some(FilePosition { file_id: FileId(0), offset: 36 })}
                {label:my_map/0 #{{}}, kind:Map, contents:Snippet("{}"), position:Some(FilePosition { file_id: FileId(0), offset: 36 })}"#]],
        );
    }

    #[test]
    fn test_arity_1_map_type() {
        check(
            r#"
         //- expect_parse_errors
         -module(test_arity_1_map_type).
         -type my_map() :: #{field1 := true }.
         foo(X) -> #~
         "#,
            None,
            expect![[r#"
                {label:my_map/0 #{field1 := Field1}, kind:Map, contents:Snippet("{field1 := ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 38 })}
                {label:my_map/0 #{field1 => Field1}, kind:Map, contents:Snippet("{field1 => ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 38 })}"#]],
        );
    }

    #[test]
    fn test_mixed_map_and_record_types() {
        check(
            r#"
         //- expect_parse_errors
         -module(test_mixed_map_and_record_types).
         -type my_map() :: #{field1 := true }.
         -record(my_record, {field1}).
         foo(X) -> #~
         "#,
            None,
            expect![[r#"
                {label:my_map/0 #{field1 := Field1}, kind:Map, contents:Snippet("{field1 := ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 48 })}
                {label:my_map/0 #{field1 => Field1}, kind:Map, contents:Snippet("{field1 => ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 48 })}
                {label:my_record, kind:Record, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_nested_map_type() {
        check(
            r#"
         //- expect_parse_errors
         -module(test_nested_map_type).
         -type my_map() :: #{field1 := true, field2 => false}.
         -type my_nested_map() :: #{field1 := #{field1 => my_map()}}.
         foo(X) -> #~
         "#,
            None,
            expect![[r#"
                {label:my_map/0 #{field1 := Field1, ... }, kind:Map, contents:Snippet("{field1 := ${1:Field1}, field2 := ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 37 })}
                {label:my_map/0 #{field1 => Field1, ... }, kind:Map, contents:Snippet("{field1 => ${1:Field1}, field2 => ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 37 })}
                {label:my_nested_map/0 #{field1 := Field1}, kind:Map, contents:Snippet("{field1 := ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 91 })}
                {label:my_nested_map/0 #{field1 => Field1}, kind:Map, contents:Snippet("{field1 => ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 91 })}"#]],
        );
    }

    #[test]
    fn test_type_from_spec() {
        check(
            r#"
         //- expect_parse_errors
         //- /src/one.erl
         -module(one).
         -type my_map(X) :: #{field1 := X}.
         -export_type([my_map/0]).

         //- /src/two.erl
         -module(two).
         -spec foo(one:my_map(integer())) -> one:my_map(integer()).
         foo(X) -> X.

         bar(X) -> X#~
         "#,
            None,
            expect![[r#"
                {label:my_map/1 #{field1 := Field1}, kind:Map, contents:Snippet("{field1 := ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 20 })}
                {label:my_map/1 #{field1 => Field1}, kind:Map, contents:Snippet("{field1 => ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 20 })}"#]],
        );
    }

    #[test]
    fn test_type_from_unowned_spec() {
        check(
            r#"
         //- expect_parse_errors
         //- /src/one.erl
         -module(one).
         -type my_map(X) :: #{field1 := X}.
         -export_type([my_map/0]).

         //- /src/two.erl
         -module(two).
         -spec foo(one:my_map(integer())) -> one:my_map(integer()).
         foo(X) -> X#~
         "#,
            None,
            expect![[r#"
                {label:my_map/1 #{field1 := Field1}, kind:Map, contents:Snippet("{field1 := ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 20 })}
                {label:my_map/1 #{field1 => Field1}, kind:Map, contents:Snippet("{field1 => ${1:Field1}}"), position:Some(FilePosition { file_id: FileId(0), offset: 20 })}"#]],
        );
    }

    #[test]
    fn test_local_type_in_function_argument() {
        check(
            r#"
         //- expect_parse_errors
         -module(test_local_type).
         -type my_map() :: #{field1 := integer(), field2 => boolean()}.
         foo(#~
         "#,
            None,
            expect![[r#"
                {label:my_map/0 #{field1 := Field1, ... }, kind:Map, contents:Snippet("{field1 := ${1:Field1}, field2 := ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 32 })}
                {label:my_map/0 #{field1 => Field1, ... }, kind:Map, contents:Snippet("{field1 => ${1:Field1}, field2 => ${2:Field2}}"), position:Some(FilePosition { file_id: FileId(0), offset: 32 })}"#]],
        );
    }
}
