/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_eqwalizer::ast::RemoteId;
use elp_ide_assists::helpers;
use elp_ide_assists::helpers::ExportForm;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::EqwalizerDiagnostic;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::NonExportedId;
use elp_types_db::eqwalizer::StructuredDiagnostic;
use hir::sema::to_def::resolve_module_name;
use hir::Name;
use hir::NameArity;
use hir::Semantic;

use crate::diagnostics::Diagnostic;
use crate::fix;

pub fn unexported_type(
    sema: &Semantic,
    file_id: FileId,
    d: &EqwalizerDiagnostic,
    diagnostic: &mut Diagnostic,
) {
    if let Some(StructuredDiagnostic::InvalidForm(Invalid::NonExportedId(NonExportedId {
        pos: _,
        id: RemoteId {
            module,
            name,
            arity,
        },
    }))) = &d.diagnostic
    {
        if let Some(module) = resolve_module_name(sema, file_id, module) {
            let name = NameArity::new(Name::from_erlang_service(name), *arity);
            if let Some(type_alias) = sema
                .db
                .def_map(module.file.file_id)
                .get_types()
                .get(&name)
                .cloned()
            {
                if !type_alias.exported {
                    let mut builder = SourceChangeBuilder::new(module.file.file_id);
                    helpers::ExportBuilder::new(
                        sema,
                        module.file.file_id,
                        ExportForm::Types,
                        &[name.clone()],
                        &mut builder,
                    )
                    .finish();
                    let edit = builder.finish();
                    diagnostic.add_fix(fix(
                        "export_type",
                        format!("Export the type `{name}`").as_str(),
                        edit,
                        diagnostic.range,
                    ))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use elp_project_model::otp::otp_supported_by_eqwalizer;
    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn unexported_type() {
        if otp_supported_by_eqwalizer() {
            check_diagnostics(
                r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
                -module(bar).

                -spec baz() -> other:a_type().
                %%             ^^^^^^^^^^^^^^ 💡 error: eqwalizer: non_exported_id
                baz() -> ok.

            //- /play/src/other.erl app:play
                -module(other).
                -type a_type() :: ok.

            "#,
            )
        }
    }

    #[test]
    fn fix_unexported_type() {
        if otp_supported_by_eqwalizer() {
            check_fix(
                r#"
            //- eqwalizer
            //- /play/src/other.erl app:play
                -module(other).
                -type a_type() :: ok.

            //- /play/src/bar.erl app:play
                -module(bar).

                -spec baz() -> other:a_~type().
                %%             ^^^^^^^^^^^^^^ 💡 error: eqwalizer: unknown_id
                baz() -> ok.
            "#,
                expect![[r#"
                    -module(other).

                -export_type([a_type/0]).
                    -type a_type() :: ok.

            "#]],
            )
        }
    }
}
