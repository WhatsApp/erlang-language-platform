/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use hir::Semantic;
use hir::known;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::NoDialyzerAttribute;
const DIAGNOSTIC_MESSAGE: &str = "Avoid -dialyzer attribute.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::Warning;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        no_dialyzer_attribute(diags, sema, file_id);
    },
};

fn no_dialyzer_attribute(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let form_list = sema.db.file_form_list(file_id);
    form_list.attributes().for_each(|(_idx, attr)| {
        if attr.name == known::dialyzer {
            if let Some(diagnostic) = make_diagnostic(sema, file_id, attr) {
                diagnostics.push(diagnostic);
            }
        }
    });
}

fn make_diagnostic(sema: &Semantic, file_id: FileId, attr: &hir::Attribute) -> Option<Diagnostic> {
    let range = attr.name_range(sema.db, file_id)?;
    let diagnostic = Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, range)
        .with_severity(DIAGNOSTIC_SEVERITY);
    Some(diagnostic)
}

#[cfg(test)]
mod tests {

    use crate::tests;

    #[test]
    fn test_dialyzer_attribute() {
        tests::check_diagnostics(
            r#"
    -module(main).
    -dialyzer({nowarn_function, foo/0}).
 %% ^^^^^^^^^ warning: Avoid -dialyzer attribute.
              "#,
        )
    }
}
