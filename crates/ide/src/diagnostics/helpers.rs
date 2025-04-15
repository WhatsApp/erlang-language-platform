/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Helpers for writing diagnostics

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use hir::FunctionDef;
use hir::Semantic;

use super::Diagnostic;
use super::Severity;
use crate::FunctionMatch;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::UseRange;
use crate::codemod_helpers::find_call_in_function;

// ---------------------------------------------------------------------

#[derive(Debug)]
pub(crate) struct DiagnosticTemplate {
    pub(crate) code: DiagnosticCode,
    pub(crate) message: String,
    pub(crate) severity: Severity,
    pub(crate) with_ignore_fix: bool,
    pub(crate) use_range: UseRange,
}

/// Define a checker for a function that should not be used. Generate
/// a diagnostic according to the template if it is found.
#[derive(Debug)]
pub(crate) struct FunctionCallDiagnostic {
    pub(crate) diagnostic_template: DiagnosticTemplate,
    pub(crate) matches: Vec<FunctionMatch>,
}

pub(crate) fn check_used_functions(
    sema: &Semantic,
    file_id: FileId,
    used_functions: &[FunctionCallDiagnostic],
    diags: &mut Vec<Diagnostic>,
) {
    let mfas: Vec<(&FunctionMatch, &DiagnosticTemplate)> = used_functions
        .iter()
        .flat_map(|u| u.matches.iter().map(|m| (m, &u.diagnostic_template)))
        .collect();
    sema.def_map(file_id)
        .get_functions()
        .for_each(|(_, def)| check_function_with_diagnostic_template(diags, sema, def, &mfas));
}

pub(crate) fn check_function_with_diagnostic_template(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    mfas: &[(&FunctionMatch, &DiagnosticTemplate)],
) {
    find_call_in_function(
        diags,
        sema,
        def,
        mfas,
        &move |ctx| Some(*ctx.t),
        &move |ctx @ MatchCtx {
                   sema,
                   def_fb,
                   extra,
                   ..
               }: MatchCtx<'_, &DiagnosticTemplate>| {
            let diag = Diagnostic::new(
                extra.code.clone(),
                extra.message.clone(),
                ctx.range(&extra.use_range),
            )
            .with_severity(extra.severity);
            let diag = if extra.with_ignore_fix {
                diag.with_ignore_fix(sema, def_fb.file_id())
            } else {
                diag
            };
            Some(diag)
        },
    );
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use super::DiagnosticTemplate;
    use super::FunctionCallDiagnostic;
    use super::check_used_functions;
    use crate::FunctionMatch;
    use crate::codemod_helpers::UseRange;
    use crate::diagnostics::AdhocSemanticDiagnostics;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::Severity;
    use crate::tests::check_diagnostics_with_config_and_ad_hoc;

    #[track_caller]
    pub(crate) fn check_diagnostics_with_ad_hoc_semantics(
        ad_hoc_semantic_diagnostics: Vec<&dyn AdhocSemanticDiagnostics>,
        fixture: &str,
    ) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config_and_ad_hoc(config, &ad_hoc_semantic_diagnostics, fixture)
    }

    #[test]
    fn unused_function() {
        check_diagnostics_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                check_used_functions(
                    sema,
                    file_id,
                    &vec![FunctionCallDiagnostic {
                        diagnostic_template: DiagnosticTemplate {
                            code: DiagnosticCode::AdHoc("a code".to_string()),
                            message: "diagnostic message".to_string(),
                            severity: Severity::Warning,
                            with_ignore_fix: true,
                            use_range: UseRange::WithArgs,
                        },
                        matches: vec![FunctionMatch::mfas("main", "foo", vec![0])]
                            .into_iter()
                            .flatten()
                            .collect(),
                    }],
                    acc,
                );
            }],
            r#"
             -module(main).
             -export([foo/0]).
             foo() -> main:foo().
             %%       ^^^^^^^^^^ 💡 warning: diagnostic message

            "#,
        )
    }
}
