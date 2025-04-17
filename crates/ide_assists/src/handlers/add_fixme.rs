/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Assist: add_fixme
//
// Adds a fixme comment above a diagnostic, prompting the user for additional reason
//
// ```
//    -dyalizer({nowarn_function, f/0}).
// %% ^^^^^^^^ 💡 w0013: misspelled attribute, saw 'dyalizer' but expected 'dialyzer'
// ```
// ->
// ```
// % elp:fixme w0013 (misspelled_attribute)
// -dyalizer({nowarn_function, f/0}).
// ```

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_ide_db::assists::AssistUserInput;
use elp_ide_db::assists::AssistUserInputType;
use elp_ide_db::assists::GroupLabel;
use elp_syntax::AstNode;
use elp_syntax::ast::edit::IndentLevel;
use elp_syntax::ast::edit::start_of_line;

use crate::AssistContext;
use crate::Assists;

pub(crate) fn add_fixme(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    for ctx_diag in ctx.diagnostics {
        if ctx_diag.allows_fixme_comment() {
            let parsed = ctx.sema.parse(ctx.frange.file_id);
            if let Some(token) = parsed
                .value
                .syntax()
                .token_at_offset(ctx_diag.range.start())
                .right_biased()
            {
                acc.add_from_diagnostic(
                    AssistId("add_fixme", AssistKind::Generate),
                    "Add fixme comment",
                    Some(GroupLabel::ignore()),
                    (*ctx_diag).clone(),
                    ctx_diag.range,
                    Some(AssistUserInput {
                        input_type: AssistUserInputType::StringAndTaskId,
                        prompt: Some("Enter reason for fixme, including task number".to_string()),
                        value: "".to_string(),
                        task_id: None,
                    }),
                    |builder| {
                        let indent = IndentLevel::from_token(&token);
                        let text = match &ctx.user_input {
                            Some(input)
                                if !input.value.is_empty()
                                    && input.task_id.is_some()
                                    && input.task_id != Some("".to_string()) =>
                            {
                                format!(
                                    "\n{}% elp:fixme {}: [{}] {}",
                                    indent,
                                    ctx_diag.code.as_labeled_code(),
                                    input.task_id.as_ref().unwrap(), // Safe because of guard
                                    input.value
                                )
                            }
                            _ => format!(
                                "\n{}% elp:fixme {} ",
                                indent,
                                ctx_diag.code.as_labeled_code(),
                            ),
                        };

                        let offset = start_of_line(&token);
                        builder.insert(offset, text.trim_end());
                    },
                );
            }
        }
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn test_base_case() {
        check_assist(
            add_fixme,
            "Add fixme comment",
            r#"
-module(main).
-dy~alizer({nowarn_function, f/0}).
%%<^^^^^^^ 💡 W0013: misspelled attribute, saw 'dyalizer' but expected 'dialyzer'
"#,
            expect![[r#"
                -module(main).
                % elp:fixme W0013 (misspelled_attribute): [T12345_test]  task edited
                -dyalizer({nowarn_function, f/0}).
            "#]],
        )
    }

    #[test]
    fn test_base_case_user_input() {
        check_assist_with_user_input_and_task_id(
            add_fixme,
            "Add fixme comment",
            "the reason is simple, blah",
            "T3333",
            r#"
-module(main).
-dy~alizer({nowarn_function, f/0}).
%%<^^^^^^ 💡 W0013: misspelled attribute, saw 'dyalizer' but expected 'dialyzer'
"#,
            expect![[r#"
                -module(main).
                % elp:fixme W0013 (misspelled_attribute): [T3333] the reason is simple, blah
                -dyalizer({nowarn_function, f/0}).
            "#]],
        )
    }
}
