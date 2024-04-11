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
use crate::helpers::ExportForm;
use crate::AssistContext;
use crate::Assists;

// Assist: export_type
//
// Export a function
//
// ```
// type foo() :: ok.
// ```
// ->
// ```
// -export_type ([foo/0]).
// type foo() :: ok.
// ```
pub(crate) fn export_type(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    if let Some(SymbolClass::Definition(SymbolDefinition::Type(type_alias))) = ctx.classify_offset()
    {
        let function_range = type_alias.range(ctx.sema.db.upcast())?;
        let name_arity = type_alias.name();
        let name_arity = (*name_arity).clone();

        if !type_alias.exported {
            let id = AssistId("export_type", AssistKind::QuickFix);
            let message = format!("Export the type `{name_arity}`");
            acc.add(id, message, None, function_range, None, |builder| {
                helpers::ExportBuilder::new(
                    &ctx.sema,
                    ctx.file_id(),
                    ExportForm::Types,
                    &[name_arity],
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
            export_type,
            "Export the type `heavy_calculations/1`",
            r#"
 -module(life).

 -type heavy_cal~culations(X) :: X.
"#,
            expect![[r#"
                -module(life).

                -export_type([heavy_calculations/1]).

                -type heavy_calculations(X) :: X.
            "#]],
        )
    }

    #[test]
    fn export_no_module_header() {
        check_assist(
            export_type,
            "Export the type `heavy_calculations/1`",
            r#"
 -type heavy_cal~culations(X) :: X.
"#,
            expect![[r#"

                -export_type([heavy_calculations/1]).
                -type heavy_calculations(X) :: X.
            "#]],
        )
    }

    #[test]
    fn already_exported_1() {
        check_assist_not_applicable(
            export_type,
            r#"
 -export_type([heavy_calculations/1]).
 -type heavy_cal~culations(X) :: X.
"#,
        )
    }

    #[test]
    fn export_into_existing_export_if_only_one() {
        check_assist(
            export_type,
            "Export the type `heavy_calculations/1`",
            r#"
                -module(life).
                -export_type([foo/0]).

                -type heavy_cal~culations(X) :: X.
                -type foo() :: ok.
            "#,
            expect![[r#"
                -module(life).
                -export_type([foo/0, heavy_calculations/1]).

                -type heavy_calculations(X) :: X.
                -type foo() :: ok.
            "#]],
        )
    }

    #[test]
    fn export_into_existing_empty_export() {
        check_assist(
            export_type,
            "Export the type `heavy_calculations/1`",
            r#"
                -module(life).
                -export_type([]).

                -type heavy_cal~culations(X) :: X.
                -type foo() :: ok.
            "#,
            expect![[r#"
                -module(life).
                -export_type([heavy_calculations/1]).

                -type heavy_calculations(X) :: X.
                -type foo() :: ok.
            "#]],
        )
    }

    #[test]
    fn export_into_new_export_if_multiple_existing() {
        check_assist(
            export_type,
            "Export the type `heavy_calculations/1`",
            r#"
                -module(life).
                -export_type([foo/0]).
                -export_type([bar/0]).

                -type heavy_cal~culations(X) :: X.
                -type foo() :: ok.
                -type bar() :: ok.
            "#,
            expect![[r#"
                -module(life).

                -export_type([heavy_calculations/1]).
                -export_type([foo/0]).
                -export_type([bar/0]).

                -type heavy_calculations(X) :: X.
                -type foo() :: ok.
                -type bar() :: ok.
            "#]],
        )
    }

    #[test]
    fn export_quoted_atom_type() {
        check_assist(
            export_type,
            "Export the type `'Code.Navigation.Elixirish'/1`",
            r#"
                -module(life).

                -type 'Code.Navigation.Eli~xirish'(X) :: X.
                "#,
            expect![[r#"
                -module(life).

                -export_type(['Code.Navigation.Elixirish'/1]).

                -type 'Code.Navigation.Elixirish'(X) :: X.
            "#]],
        )
    }
}
