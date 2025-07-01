/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: duplicate_module
//
// Return a warning if more than one module has the same name

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::ModuleName;
use elp_syntax::AstNode;
use hir::Semantic;

use super::Diagnostic;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use crate::diagnostics::Severity;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false, // Allow duplication in test fixtures
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _file_kind| {
        check_file(diags, sema, &file_id);
    },
};

fn check_file(acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: &FileId) -> Option<()> {
    // We cannot ask for the module name from the module_index, as
    // this file_id may be discarded as a duplicate
    let module_name_ast = sema.module_attribute_name(*file_id)?;
    let module_name = ModuleName::new(module_name_ast.text()?.as_str());

    let app_data = sema.db.file_app_data(*file_id)?;
    let module_index = sema.db.module_index(app_data.project_id);
    if let Some(_dups) = module_index.duplicates(&module_name) {
        let range = module_name_ast.syntax().text_range();
        acc.push(
            Diagnostic::new(
                DiagnosticCode::DuplicateModule,
                "Duplicate module name",
                range,
            )
            .with_severity(Severity::Warning),
        );
    }
    Some(())
}

#[cfg(test)]
mod test {
    use crate::tests::check_diagnostics;

    #[test]
    fn duplicate_modules() {
        check_diagnostics(
            r#"
             //- /src/dup_mod.erl
             -module(dup_mod).
                  %% ^^^^^^^ warning: Duplicate module name

             //- /src/sub/dup_mod.erl
             -module(dup_mod).
                  %% ^^^^^^^ warning: Duplicate module name
            "#,
        )
    }
}
