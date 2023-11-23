/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;

use crate::helpers;
use crate::AssistContext;
use crate::Assists;

// Assist: export_function
//
// Export a function if it is unused
//
// ```
// foo() -> ok.
// ```
// ->
// ```
// -export([foo/0]).
// foo() -> ok.
// ```
pub(crate) fn export_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    if let Some(SymbolClass::Definition(SymbolDefinition::Function(fun))) = ctx.classify_offset() {
        let function_range = fun.range(ctx.sema.db.upcast())?;
        let function_name_arity = fun.name;

        if !fun.exported {
            let id = AssistId("export_function", AssistKind::QuickFix);
            let message = format!("Export the function `{function_name_arity}`");
            acc.add(id, message, function_range, None, |builder| {
                helpers::ExportBuilder::new(
                    &ctx.sema,
                    ctx.file_id(),
                    &[function_name_arity],
                    builder,
                )
                .finish();
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
    fn export_with_module_header() {
        check_assist(
            export_function,
            "Export the function `heavy_calculations/1`",
            r#"
 -module(life).

 heavy_cal~culations(X) -> X.
"#,
            expect![[r#"
                -module(life).

                -export([heavy_calculations/1]).

                heavy_calculations(X) -> X.
            "#]],
        )
    }

    #[test]
    fn export_no_module_header() {
        check_assist(
            export_function,
            "Export the function `heavy_calculations/1`",
            r#"
 heavy_cal~culations(X) -> X.
"#,
            expect![[r#"

                -export([heavy_calculations/1]).
                heavy_calculations(X) -> X.
            "#]],
        )
    }

    #[test]
    fn already_exported_1() {
        check_assist_not_applicable(
            export_function,
            r#"
 -export([heavy_calculations/1]).
 heavy_cal~culations(X) -> X.
"#,
        )
    }

    #[test]
    fn already_exported_2() {
        check_assist_not_applicable(
            export_function,
            r#"
 -compile([export_all]).
 heavy_cal~culations(X) -> X.
"#,
        )
    }

    #[test]
    fn export_into_existing_export_if_only_one() {
        check_assist(
            export_function,
            "Export the function `heavy_calculations/1`",
            r#"
                -module(life).
                -export([foo/0]).

                heavy_cal~culations(X) -> X.
                foo() -> ok.
            "#,
            expect![[r#"
                -module(life).
                -export([foo/0, heavy_calculations/1]).

                heavy_calculations(X) -> X.
                foo() -> ok.
            "#]],
        )
    }

    #[test]
    fn export_into_existing_empty_export() {
        check_assist(
            export_function,
            "Export the function `heavy_calculations/1`",
            r#"
                -module(life).
                -export([]).

                heavy_cal~culations(X) -> X.
                foo() -> ok.
            "#,
            expect![[r#"
                -module(life).
                -export([heavy_calculations/1]).

                heavy_calculations(X) -> X.
                foo() -> ok.
            "#]],
        )
    }

    #[test]
    fn export_into_new_export_if_multiple_existing() {
        check_assist(
            export_function,
            "Export the function `heavy_calculations/1`",
            r#"
                -module(life).
                -export([foo/0]).
                -export([bar/0]).

                heavy_cal~culations(X) -> X.
                foo() -> ok.
                bar() -> ok.
            "#,
            expect![[r#"
                -module(life).

                -export([heavy_calculations/1]).
                -export([foo/0]).
                -export([bar/0]).

                heavy_calculations(X) -> X.
                foo() -> ok.
                bar() -> ok.
            "#]],
        )
    }

    #[test]
    fn export_quoted_atom_function() {
        check_assist(
            export_function,
            "Export the function `'Code.Navigation.Elixirish'/1`",
            r#"
                -module(life).

                'Code.Navigation.Eli~xirish'(X) -> X.
                "#,
            expect![[r#"
                -module(life).

                -export(['Code.Navigation.Elixirish'/1]).

                'Code.Navigation.Elixirish'(X) -> X.
            "#]],
        )
    }

    #[test]
    fn export_cursor_on_left_margin() {
        check_assist(
            export_function,
            "Export the function `heavy_calculations/1`",
            r#"
-module(life).

~heavy_calculations(X) -> X.
"#,
            expect![[r#"
                -module(life).

                -export([heavy_calculations/1]).

                heavy_calculations(X) -> X.
            "#]],
        )
    }
}
