/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::assists::AssistContextDiagnosticCode;
use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::AstNode;
use elp_syntax::ast;

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;

// Assist: ignore_variable
//
// Prepend an underscore to a variable
//
// ```
// meaning_of_life() ->
//   Thoughts = thinking(),
//   42.
// ```
// ->
// ```
// meaning_of_life() ->
//   _Thoughts = thinking(),
//   42.
// ```
pub(crate) fn ignore_variable(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    for d in ctx.diagnostics {
        if let AssistContextDiagnosticCode::UnusedVariable = d.code {
            let var: ast::Var = ctx.find_node_at_custom_offset::<ast::Var>(d.range.start())?;
            let var_name = var.text();
            let var_range = var.syntax().text_range();
            acc.add_from_diagnostic(
                AssistId("ignore_variable", AssistKind::QuickFix),
                format!("Prefix the variable name with an underscore: `_{var_name}`"),
                None,
                (*d).clone(),
                var_range,
                None,
                |builder| {
                    builder.edit_file(ctx.frange.file_id);
                    builder.insert(var.syntax().text_range().start(), "_")
                },
            );
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
    fn test_ignore_unused_variable() {
        check_assist(
            ignore_variable,
            "Prefix the variable name with an underscore: `_Thoughts`",
            r#"
-module(my_module).
-export([meaning_of_life/0]).
meaning_of_life() ->
    Th~oughts = thinking(),
 %% ^^^^^^^^^ 💡 L1268: variable 'Thoughts' is unused
    42.
"#,
            expect![[r#"
            -module(my_module).
            -export([meaning_of_life/0]).
            meaning_of_life() ->
                _Thoughts = thinking(),
                42.
            "#]],
        )
    }

    #[test]
    fn test_ignore_already_unused_variable() {
        check_assist_not_applicable(
            ignore_variable,
            r#"
-module(my_module).
-export([meaning_of_life/0]).
meaning_of_life() ->
    _Thou~ghts = thinking(),
    42.
"#,
        );
    }

    #[test]
    fn test_unknown_diagnostic() {
        check_assist(
            ignore_variable,
            "Prefix the variable name with an underscore: `_Thoughts`",
            r#"
-module(my_module).
     %% ^^^^^^^^^ 💡 X12345: Module name does not match file name
-export([meaning_of_life/0]).
meaning_of_life() ->
    Th~oughts = thinking(),
 %% ^^^^^^^^^ 💡 L1268: variable 'Thoughts' is unused
    42.
"#,
            expect![[r#"
            -module(my_module).
            -export([meaning_of_life/0]).
            meaning_of_life() ->
                _Thoughts = thinking(),
                42.
            "#]],
        )
    }
}
