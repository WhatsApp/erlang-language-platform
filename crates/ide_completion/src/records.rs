/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_syntax::AstNode;
use elp_syntax::algo;
use elp_syntax::ast;
use hir::InFile;
use hir::Name;

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;
use crate::Kind;

pub(crate) fn add_completions(acc: &mut Vec<Completion>, ctx: &Ctx) -> DoneFlag {
    add_in_create_or_update(acc, ctx) || add_token_based_completions(acc, ctx)
}

/// #rec{field1~} or X#rec{field1~}
pub(crate) fn add_in_create_or_update(
    acc: &mut Vec<Completion>,
    Ctx {
        file_position,
        parsed,
        sema,
        trigger,
        ..
    }: &Ctx,
) -> DoneFlag {
    let node = parsed.value.syntax();
    match trigger {
        Some('#') | None => (),
        _ => return false,
    };

    match algo::find_node_at_offset::<ast::RecordExpr>(node, file_position.offset)
        .and_then(|e| e.name())
        .or_else(|| {
            algo::find_node_at_offset::<ast::RecordUpdateExpr>(node, file_position.offset)
                .and_then(|e| e.name())
        }) {
        None => false,
        Some(record_name) => {
            || -> Option<()> {
                let record = sema.to_def(InFile::new(file_position.file_id, &record_name))?;
                let field =
                    algo::find_node_at_offset::<ast::RecordField>(node, file_position.offset)?;
                let prefix = &field.name()?.text()?;
                let completions = record
                    .field_names(sema.db)
                    .filter(|field_name| field_name.starts_with(prefix))
                    .map(field_name_to_completion_with_equals);

                acc.extend(completions);
                Some(())
            }();
            true
        }
    }
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
    let add_record_name_completions = |name_prefix: &str, acc: &mut Vec<Completion>| {
        let def_map = sema.def_map(file_position.file_id);
        let completions = def_map
            .get_records()
            .iter()
            .filter(|(name, _)| name.starts_with(name_prefix))
            .map(|(name, _)| Completion {
                label: name.to_quoted_string().into_owned(),
                kind: Kind::Record,
                contents: Contents::SameAsLabel,
                position: None,
                sort_text: None,
                deprecated: false,
                additional_edit: None,
            });
        acc.extend(completions);
        true
    };
    let add_record_index_completions =
        |rec_name: &str, field_prefix: &str, acc: &mut Vec<Completion>| {
            let def_map = sema.def_map(file_position.file_id);
            let record_opt = def_map
                .get_records()
                .iter()
                .find(|(name, _)| name.as_str() == rec_name)
                .map(|(_, rec)| rec);
            if let Some(record) = record_opt {
                let completions = record
                    .field_names(sema.db)
                    .filter(|name| name.as_str().starts_with(field_prefix))
                    .map(field_name_to_completion);
                acc.extend(completions);
                true
            } else {
                false
            }
        };

    use elp_syntax::SyntaxKind as K;
    let default = vec![];
    let previous_tokens: &[_] = previous_tokens.as_ref().unwrap_or(&default);
    match previous_tokens {
        // // #rec_name_prefix~
        [.., (K::ANON_POUND, _), (K::ATOM, rec_name_prefix)]
            if matches!(trigger, Some('#') | None) =>
        {
            add_record_name_completions(rec_name_prefix.text(), acc)
        }
        // // #~
        [.., (K::ANON_POUND, _)] if matches!(trigger, Some('#') | None) => {
            add_record_name_completions("", acc)
        }
        // #rec_name.field_prefix
        [
            ..,
            (K::ANON_POUND, _),
            (K::ATOM, rec_name),
            (K::ANON_DOT, _),
            (K::ATOM, field_prefix),
        ] if matches!(trigger, Some('.') | None) => {
            add_record_index_completions(rec_name.text(), field_prefix.text(), acc)
        }
        // #rec_name.
        [
            ..,
            (K::ANON_POUND, _),
            (K::ATOM, rec_name),
            (K::ANON_DOT, _),
        ] if matches!(trigger, Some('.') | None) => {
            add_record_index_completions(rec_name.text(), "", acc)
        }
        // #rec_name{field_prefix~
        [
            ..,
            (K::ANON_POUND, _),
            (K::ATOM, rec_name),
            (K::ANON_LBRACE, _),
            (K::ATOM, rec_field_prefix),
        ] if matches!(trigger, Some('#') | None) => {
            add_record_index_completions(rec_name.text(), rec_field_prefix.text(), acc)
        }
        _ => false,
    }
}

fn field_name_to_completion_with_equals(field_name: Name) -> Completion {
    Completion {
        label: field_name.to_string(),
        kind: Kind::RecordField,
        contents: Contents::String(format!("{} = ", &field_name)),
        position: None,
        sort_text: None,
        deprecated: false,
        additional_edit: None,
    }
}

fn field_name_to_completion(field_name: Name) -> Completion {
    Completion {
        label: field_name.to_string(),
        kind: Kind::RecordField,
        contents: Contents::SameAsLabel,
        position: None,
        sort_text: None,
        deprecated: false,
        additional_edit: None,
    }
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
    fn test_record_index() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::FIELD).unwrap() == "5");

        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo() -> _ = #rec.f~.
        "#,
            Some('.'),
            expect![[r#"
                {label:field1, kind:RecordField, contents:SameAsLabel, position:None}
                {label:field2, kind:RecordField, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo() -> _ = #rec.f~.
        "#,
            None,
            expect![[r#"
                {label:field1, kind:RecordField, contents:SameAsLabel, position:None}
                {label:field2, kind:RecordField, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_record_field() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::FIELD).unwrap() == "5");

        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo(X) -> _ = X#rec.f~.
        "#,
            Some('.'),
            expect![[r#"
                {label:field1, kind:RecordField, contents:SameAsLabel, position:None}
                {label:field2, kind:RecordField, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo(X) -> _ = X#rec.f~.
        "#,
            Some('.'),
            expect![[r#"
                {label:field1, kind:RecordField, contents:SameAsLabel, position:None}
                {label:field2, kind:RecordField, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_field_in_create() {
        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo() -> #rec{fie~=3}.
        "#,
            None,
            expect![[r#"
                {label:field1, kind:RecordField, contents:String("field1 = "), position:None}
                {label:field2, kind:RecordField, contents:String("field2 = "), position:None}"#]],
        );

        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo() -> #rec{fie~=3}.
        "#,
            Some('#'),
            expect![[r#"
                {label:field1, kind:RecordField, contents:String("field1 = "), position:None}
                {label:field2, kind:RecordField, contents:String("field2 = "), position:None}"#]],
        );
    }

    #[test]
    fn test_field_in_update() {
        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo(X) -> X#rec{fie~=3}.
        "#,
            None,
            expect![[r#"
                {label:field1, kind:RecordField, contents:String("field1 = "), position:None}
                {label:field2, kind:RecordField, contents:String("field2 = "), position:None}"#]],
        );

        check(
            r#"
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo(X) -> X#rec{fie~=3}.
        "#,
            Some('#'),
            expect![[r#"
                {label:field1, kind:RecordField, contents:String("field1 = "), position:None}
                {label:field2, kind:RecordField, contents:String("field2 = "), position:None}"#]],
        );
    }

    #[test]
    fn test_record_name() {
        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(this_record, {field1=1, field2=2}).
        -record(that_record, {}).
        -record(another, {}).
        foo(X) -> #th~
        "#,
            None,
            expect![[r#"
                {label:that_record, kind:Record, contents:SameAsLabel, position:None}
                {label:this_record, kind:Record, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(this_record, {field1=1, field2=2}).
        -record(that_record, {}).
        -record(another, {}).
        foo(X) -> #~
        "#,
            None,
            expect![[r#"
                {label:another, kind:Record, contents:SameAsLabel, position:None}
                {label:that_record, kind:Record, contents:SameAsLabel, position:None}
                {label:this_record, kind:Record, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_quoted_record_name() {
        // Irregular names are quoted.
        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record('this.record', {field1=1, field2=2}).
        -record('that$record', {}).
        foo(X) -> #~
        "#,
            None,
            expect![[r#"
                {label:'that$record', kind:Record, contents:SameAsLabel, position:None}
                {label:'this.record', kind:Record, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_record_error_recovery() {
        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(rec, {field1=1, field2=2}).
        foo(X) -> #rec{field1 = 1, field2~.
        "#,
            None,
            expect![[
                r#"{label:field2, kind:RecordField, contents:String("field2 = "), position:None}"#
            ]],
        );

        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(rec, {field1=1, field2=2}).
        foo(X) -> X#rec{field1 = 1, field2~.
        "#,
            None,
            expect![[
                r#"{label:field2, kind:RecordField, contents:String("field2 = "), position:None}"#
            ]],
        );

        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(rec, {field1=1, field2=2}).
        foo(X) -> case ok of
            ok -> #r~
        "#,
            None,
            expect!["{label:rec, kind:Record, contents:SameAsLabel, position:None}"],
        );

        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(rec, {field1=1, field2=2}).
        foo(X) -> case ok of
            ok -> #rec.~
        "#,
            None,
            expect![[r#"
                {label:field1, kind:RecordField, contents:SameAsLabel, position:None}
                {label:field2, kind:RecordField, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(rec, {field1=1, field2=2}).
        foo(X) -> case ok of
            ok -> X#rec{field1 = 1, field2~}.
        "#,
            None,
            expect![[
                r#"{label:field2, kind:RecordField, contents:String("field2 = "), position:None}"#
            ]],
        );
    }

    #[test]
    fn test_record_name_in_function_signature() {
        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(this_record, {field1=1, field2=2}).
        -record(that_record, {}).
        -record(another, {}).
        foo(#th~
        "#,
            None,
            expect![[r#"
                {label:that_record, kind:Record, contents:SameAsLabel, position:None}
                {label:this_record, kind:Record, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_record_field_in_function_signature() {
        check(
            r#"
        //- expect_parse_errors
        -module(sample).
        -record(rec, {field1, field2, other}).
        foo(#rec{fie~
        "#,
            None,
            expect![[r#"
                {label:field1, kind:RecordField, contents:SameAsLabel, position:None}
                {label:field2, kind:RecordField, contents:SameAsLabel, position:None}"#]],
        );
    }
}
