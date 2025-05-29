/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::AstNode;
use elp_syntax::algo;
use elp_syntax::ast;
use hir::MacroName;
use hir::Name;
use hir::known;

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;
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
                .map(macro_name_to_completion);

            acc.extend(user_defined);

            let built_in = BUILT_IN;
            let predefined = built_in
                .iter()
                .filter(|name| name.starts_with(&prefix))
                .map(built_in_macro_name_to_completion);
            acc.extend(predefined);

            // If we have a trigger character, it means we are completing a macro name. No need to compute other completions.
            trigger.is_some()
        }
    }
}

fn macro_name_to_completion(macro_name: &MacroName) -> Completion {
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
                additional_edit: None,
            }
        }
        None => Completion {
            label: macro_name.to_string(),
            kind: Kind::Macro,
            contents: Contents::SameAsLabel,
            position: None,
            sort_text: None,
            deprecated: false,
            additional_edit: None,
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
}
