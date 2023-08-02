/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistContextDiagnosticCode;
use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::ast::HasArity;
use elp_syntax::ast::{self};

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;
use crate::helpers::ranges_for_delete_function;

// Assist: delete_function
//
// Delete a function, if deemed as unused.
//
// ```
// -module(life).
//
// heavy_calculations(X) -> X.
// %% ^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
//
// meaning() ->
//   42.
// ```
// ->
// ```
// -module(life).
//
// meaning() ->
//   42.
// ```
pub(crate) fn delete_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    for d in ctx.diagnostics {
        if let AssistContextDiagnosticCode::UnusedFunction = d.code {
            let function_declaration: ast::FunDecl =
                ctx.find_node_at_custom_offset::<ast::FunDecl>(d.range.start())?;
            let function_name = function_declaration.name()?;
            let function_arity = function_declaration.arity_value()?;
            let function_ranges = ranges_for_delete_function(ctx, &function_declaration)?;

            let id = AssistId("delete_function", AssistKind::QuickFix);
            let message = format!("Remove the unused function `{function_name}/{function_arity}`");
            acc.add(id, message, function_ranges.function, None, |builder| {
                builder.edit_file(ctx.frange.file_id);
                function_ranges.delete(builder);
            });
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
    fn test_delete_unused_function() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 heavy_cal~culations(X) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   X.

 meaning() ->
   42.
"#,
            expect![[r#"
                -module(life).

                meaning() ->
                  42.
            "#]],
        )
    }

    #[test]
    fn test_delete_unused_function_multiple_clauses() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 heavy_cal~culations(0) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   0;
 heavy_calculations(X) ->
   X.

 meaning() ->
   42.
"#,
            expect![[r#"
                -module(life).

                meaning() ->
                  42.
            "#]],
        )
    }

    #[test]
    fn test_delete_unused_function_with_spec_1() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 -spec heavy_calculations(any()) -> any().
 heavy_cal~culations(X) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   X.

 meaning() ->
   42.
"#,
            expect![[r#"
                -module(life).

                meaning() ->
                  42.
            "#]],
        )
    }

    #[test]
    fn test_delete_unused_function_with_spec_2() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 heavy_cal~culations(X) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   X.

 meaning() ->
   42.
 -spec heavy_calculations(any()) -> any().
"#,
            expect![[r#"
                -module(life).

                meaning() ->
                  42.
            "#]],
        )
    }

    #[test]
    fn delete_unused_function_with_edoc_1() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 %% @doc some docs
 heavy_cal~culations(X) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   X.

 meaning() ->
   42.
 -spec heavy_calculations(any()) -> any().
"#,
            expect![[r#"
                -module(life).

                meaning() ->
                  42.
            "#]],
        )
    }

    #[test]
    fn delete_unused_function_with_edoc_2() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 %% @doc some docs
 %% Continue the tag, then a gap

 heavy_cal~culations(X) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   X.

 meaning() ->
   42.
 -spec heavy_calculations(any()) -> any().
"#,
            expect![[r#"
                -module(life).

                meaning() ->
                  42.
            "#]],
        )
    }

    #[test]
    fn delete_unused_function_with_edoc_3() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 %% This is not part of the edoc
 %% @doc some docs
 heavy_cal~culations(X) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   X.

 meaning() ->
   42.
 -spec heavy_calculations(any()) -> any().
"#,
            expect![[r#"
                -module(life).

                %% This is not part of the edoc
                meaning() ->
                  42.
            "#]],
        )
    }

    #[test]
    fn delete_unused_function_with_edoc_interspersed() {
        check_assist(
            delete_function,
            "Remove the unused function `heavy_calculations/1`",
            r#"
 -module(life).

 %% This is not part of the edoc
 %% @doc some docs
 -type client2() :: #client2{}.
 %% The above type does not stop this being part of the edoc
 heavy_cal~culations(X) ->
 %% ^^^^^^^^^^^^^^^^ 💡 L1230: Function heavy_calculations/1 is unused
   X.

 meaning() ->
   42.
 -spec heavy_calculations(any()) -> any().
"#,
            expect![[r#"
                -module(life).

                %% This is not part of the edoc
                -type client2() :: #client2{}.
                meaning() ->
                  42.
            "#]],
        )
    }
}
