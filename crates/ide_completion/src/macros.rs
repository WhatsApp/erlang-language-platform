/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::algo;
use elp_syntax::ast;
use hir::MacroName;
use hir::Name;
use hir::Semantic;
use hir::known;
use lazy_static::lazy_static;

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;
use crate::IncludeFile;
use crate::Kind;
use crate::helpers;

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Ctx {
        file_position,
        parsed,
        sema,
        trigger,
        ..
    }: &Ctx,
) -> DoneFlag {
    match trigger {
        Some('?') | None => (),
        _ => return false,
    };

    let node = parsed.value.syntax();
    match algo::find_node_at_offset::<ast::MacroCallExpr>(node, file_position.offset) {
        None => false,
        Some(call) => {
            let prefix =
                match algo::find_node_at_offset::<ast::MacroName>(node, file_position.offset) {
                    Some(prefix) => prefix.text(),
                    None => call.name().map(|n| n.to_string()),
                }
                .unwrap_or_default();
            let def_map = sema.def_map(file_position.file_id);
            let user_defined = def_map
                .get_macros()
                .keys()
                .filter(|macro_name| macro_name.name().starts_with(&prefix))
                .map(|name| macro_name_to_completion(sema, file_position.file_id, name, None));

            acc.extend(user_defined);

            let built_in = BUILT_IN;
            let predefined = built_in
                .iter()
                .filter(|name| name.starts_with(&prefix))
                .map(built_in_macro_name_to_completion);
            acc.extend(predefined);

            let well_known_macros = &WELL_KNOWN_MACROS;
            let well_known = well_known_macros
                .iter()
                .filter(|(name, _include)| (*name).starts_with(&prefix))
                .map(|(name, include)| {
                    well_known_macro_name_to_completion(sema, file_position.file_id, name, include)
                });
            acc.extend(well_known);

            let known_macros = macro_index_completion(sema, file_position.file_id, &prefix);
            acc.extend(known_macros);

            // If we have a trigger character, it means we are completing a macro name. No need to compute other completions.
            trigger.is_some()
        }
    }
}

fn macro_index_completion(sema: &Semantic, file_id: FileId, prefix: &str) -> Vec<Completion> {
    if let Some(project_id) = sema.db.file_project_id(file_id) {
        let index = sema.macro_define_index(project_id);
        index
            .complete(prefix)
            .iter()
            .map(|(_chars, define)| {
                let form_list = sema.form_list(define.file_id);
                let define = &form_list[define.value];
                macro_name_to_completion(sema, file_id, &define.name, None)
            })
            .collect()
    } else {
        vec![]
    }
}

fn macro_name_to_completion(
    sema: &Semantic,
    file_id: FileId,
    macro_name: &MacroName,
    include: Option<IncludeFile>,
) -> Completion {
    let additional_edit = if let Some(inc) = include {
        inc.insert_position_if_needed(sema, file_id)
            .map(|pos| (pos, inc.clone()))
    } else {
        None
    };
    match macro_name.arity() {
        Some(arity) => {
            let label = macro_name.to_string();
            let contents = helpers::format_call(macro_name.name(), arity);
            Completion {
                label,
                kind: Kind::Macro,
                contents,
                position: None,
                sort_text: None,
                deprecated: false,
                additional_edit,
            }
        }
        None => Completion {
            label: macro_name.to_string(),
            kind: Kind::Macro,
            contents: Contents::SameAsLabel,
            position: None,
            sort_text: None,
            deprecated: false,
            additional_edit,
        },
    }
}

fn built_in_macro_name_to_completion(name: &Name) -> Completion {
    Completion {
        label: name.to_string(),
        kind: Kind::Macro,
        contents: Contents::SameAsLabel,
        position: None,
        sort_text: None,
        deprecated: false,
        additional_edit: None,
    }
}

const BUILT_IN: [Name; 8] = [
    known::FILE,
    known::FUNCTION_NAME,
    known::FUNCTION_ARITY,
    known::LINE,
    known::MODULE,
    known::MODULE_STRING,
    known::MACHINE,
    known::OTP_RELEASE,
];

fn well_known_macro_name_to_completion(
    sema: &Semantic,
    file_id: FileId,
    name: &Name,
    include: &IncludeFile,
) -> Completion {
    let additional_edit = include
        .insert_position_if_needed(sema, file_id)
        .map(|pos| (pos, include.clone()));
    Completion {
        label: name.to_string(),
        kind: Kind::Macro,
        contents: Contents::SameAsLabel,
        position: None,
        sort_text: None,
        deprecated: false,
        additional_edit,
    }
}

lazy_static! {
    static ref INCLUDE_ASSERT: IncludeFile = IncludeFile {
        include_lib: true,
        path: "stdlib/include/assert.hrl".to_string(),
    };
}

lazy_static! {
    pub static ref WELL_KNOWN_MACROS: Vec<(Name, IncludeFile)> = vec![
        (known::assertEqual, INCLUDE_ASSERT.clone()),
        (known::assertEqualSorted, INCLUDE_ASSERT.clone()),
        (known::assertMatch, INCLUDE_ASSERT.clone()),
    ]
    .into_iter()
    .collect();
}

#[cfg(test)]
mod test {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::tests::get_completions;
    use crate::tests::render_completions;

    fn check(code: &str, trigger: Option<char>, expect: Expect) {
        let completions = get_completions(code, trigger);
        let actual = &render_completions(completions);
        expect.assert_eq(actual);
    }

    #[test]
    fn test_user_defined_macros() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");

        check(
            r#"
    -module(sample1).
    -define(FOO, 1).
    -define(FOO(), 1).
    -define(FOO(X, Y, Z), {X, Y, Z}).
    -define(FOB, 1).
    -define(BAR, 1).
    foo() -> ?FO~
    "#,
            Some('?'),
            expect![[r#"
                {label:FOB, kind:Macro, contents:SameAsLabel, position:None}
                {label:FOO, kind:Macro, contents:SameAsLabel, position:None}
                {label:FOO/0, kind:Macro, contents:Snippet("FOO()"), position:None}
                {label:FOO/3, kind:Macro, contents:Snippet("FOO(${1:Arg1}, ${2:Arg2}, ${3:Arg3})"), position:None}"#]],
        );

        check(
            r#"
    -module(sample1).
    -define(FOO, 1).
    -define(BAR, 1).
    foo() -> ?FO~
    "#,
            None,
            expect!["{label:FOO, kind:Macro, contents:SameAsLabel, position:None}"],
        );

        check(
            r#"
    -module(sample1).
    -define(FOO, ok).
    -define(BAR, 1).
    spec foo() -> ?FO~.
    foo() -> ok.
    "#,
            None,
            expect!["{label:FOO, kind:Macro, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn test_predefined_macros() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
    -module(sample1).
    -define(F_USER_DEFINED, 1).
    foo() -> ?F~
    "#,
            Some('?'),
            expect![[r#"
                {label:FILE, kind:Macro, contents:SameAsLabel, position:None}
                {label:FUNCTION_ARITY, kind:Macro, contents:SameAsLabel, position:None}
                {label:FUNCTION_NAME, kind:Macro, contents:SameAsLabel, position:None}
                {label:F_USER_DEFINED, kind:Macro, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
    -module(sample1).
    foo() -> ?M~
    "#,
            Some('?'),
            expect![[r#"
                {label:MACHINE, kind:Macro, contents:SameAsLabel, position:None}
                {label:MODULE, kind:Macro, contents:SameAsLabel, position:None}
                {label:MODULE_STRING, kind:Macro, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
    -module(sample1).
    foo() -> ?L~
    "#,
            Some('?'),
            expect!["{label:LINE, kind:Macro, contents:SameAsLabel, position:None}"],
        );

        check(
            r#"
    -module(sample1).
    foo() -> ?O~
    "#,
            Some('?'),
            expect!["{label:OTP_RELEASE, kind:Macro, contents:SameAsLabel, position:None}"],
        );
    }

    // -----------------------------------------------------------------

    #[test]
    fn well_known_macros_import() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
    -module(sample1).
    foo() -> ?asse~
    "#,
            Some('?'),
            expect![[r#"
                {label:assertEqual, kind:Macro, contents:SameAsLabel, position:None, include:18:"-include_lib(\"stdlib/include/assert.hrl\")."}
                {label:assertEqualSorted, kind:Macro, contents:SameAsLabel, position:None, include:18:"-include_lib(\"stdlib/include/assert.hrl\")."}
                {label:assertMatch, kind:Macro, contents:SameAsLabel, position:None, include:18:"-include_lib(\"stdlib/include/assert.hrl\")."}"#]],
        );
    }

    #[test]
    fn well_known_macros_no_import() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
    -module(sample1).
    -include_lib("stdlib/include/assert.hrl").
    foo() -> ?asse~
    "#,
            Some('?'),
            expect![[r#"
                {label:assertEqual, kind:Macro, contents:SameAsLabel, position:None}
                {label:assertEqualSorted, kind:Macro, contents:SameAsLabel, position:None}
                {label:assertMatch, kind:Macro, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn well_known_macros_no_match() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
    -module(sample1).
    foo() -> ?assb~
    "#,
            Some('?'),
            expect![""],
        );
    }

    #[test]
    fn detect_macros_match() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
         //- /src/sample1.erl
           -module(sample1).
           foo() -> ?FO~
         //- /src/header.hrl
           -define(FOO,3).
           -define(FOO(X),X+3).
    "#,
            Some('?'),
            expect![[r#"
                {label:FOO, kind:Macro, contents:SameAsLabel, position:None}
                {label:FOO/1, kind:Macro, contents:Snippet("FOO(${1:Arg1})"), position:None}"#]],
        );
    }
}
