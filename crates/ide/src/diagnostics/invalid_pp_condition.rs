/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Diagnostic: invalid-pp-condition (W0061)
//!
//! Reports invalid preprocessor conditions that cannot be evaluated.
//! This includes unsupported operators (like xor) or function calls.

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use hir::PPCondition;
use hir::Semantic;

use super::compute_pp_condition_range;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct InvalidPPConditionLinter;

/// Context stores pre-computed message since match_description has no db access
#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) struct InvalidPPConditionContext {
    pub message: String,
}

impl Linter for InvalidPPConditionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::HirInvalidPPCondition
    }

    fn description(&self) -> &'static str {
        "invalid preprocessor condition"
    }

    fn is_enabled(&self) -> bool {
        false
    }
}

impl GenericLinter for InvalidPPConditionLinter {
    type Context = InvalidPPConditionContext;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        // Only collect diagnostics from the main file if it's not a header file.
        // Header files get their diagnostics reported via W0062 in the including files.
        let file_kind = sema.db.file_kind(file_id);
        if file_kind == FileKind::Header {
            return Some(Vec::new());
        }

        let form_list = sema.form_list(file_id);
        let mut matches = Vec::new();

        // Get all condition diagnostics from the preprocessor analysis
        let env = sema.db.project_macro_environment(file_id);
        let (_, diagnostics_map) = sema
            .db
            .file_preprocessor_analysis_with_diagnostics(file_id, env);

        for (cond_id, condition) in form_list.pp_conditions() {
            // Only -if and -elif conditions can generate diagnostics
            // (they have expressions that need to be evaluated)
            let has_expr = matches!(condition, PPCondition::If { .. } | PPCondition::Elif { .. });

            if has_expr {
                // Get diagnostics from the map
                if let Some(cond_diagnostics) = diagnostics_map.get(&cond_id) {
                    for cond_diag in cond_diagnostics {
                        let range = compute_pp_condition_range(sema.db, file_id, cond_id);
                        matches.push(GenericLinterMatchContext {
                            range: FileRange { file_id, range },
                            context: InvalidPPConditionContext {
                                message: cond_diag.message.clone(),
                            },
                        });
                    }
                }
            }
        }

        Some(matches)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(context.message.clone())
    }
}

pub static LINTER: InvalidPPConditionLinter = InvalidPPConditionLinter;

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn invalid_pp_condition_same_file_w0061() {
        // Test W0061: invalid preprocessor condition in the same file
        // The condition uses an unsupported function call
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if(some_function()).
%% ^^^^^^^^^^^^^^^^^ 💡 warning: W0061: function call 'some_function' is not supported in preprocessor conditions
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_xor_operator_w0061() {
        // Test W0061: xor operator in condition
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if(true xor false).
%% ^^^^^^^^^^^^^^^^ 💡 warning: W0061: the 'xor' operator is not supported in preprocessor conditions
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_list_operation_w0061() {
        // Test W0061: list append operator in condition
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if([1] ++ [2]).
%% ^^^^^^^^^^^^ 💡 warning: W0061: list operations and send are not supported in preprocessor conditions
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_undefined_macro_w0061() {
        // Test W0061: undefined macro in condition
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if(?UNDEFINED_MACRO).
%% ^^^^^^^^^^^^^^^^^^ 💡 warning: W0061: undefined macro 'UNDEFINED_MACRO' in preprocessor condition
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_unsupported_expr_w0061() {
        // Test W0061: unsupported expression (tuple) in condition
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if({1, 2}).
%% ^^^^^^^^ 💡 warning: W0061: unsupported expression in preprocessor condition
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_defined_multiple_args_w0061() {
        // Test W0061: defined() with wrong argument count
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if(defined(FOO, BAR)).
%% ^^^^^^^^^^^^^^^^^^^ 💡 warning: W0061: defined() requires exactly one argument
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_defined_invalid_arg_w0061() {
        // Test W0061: defined() with invalid argument type
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if(defined(123)).
%% ^^^^^^^^^^^^^^ 💡 warning: W0061: defined() argument must be an atom or macro name
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_elif_w0061() {
        // Test W0061: invalid condition in -elif
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-ifdef(SOME_MACRO).
-define(FOO, 1).
-elif(some_function()).
%%   ^^^^^^^^^^^^^^^^^ 💡 warning: W0061: function call 'some_function' is not supported in preprocessor conditions
-define(FOO, 2).
-endif.

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_remote_call_w0061() {
        // Test W0061: remote function call in condition
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirInvalidPPCondition)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

-if(lists:member(1, [1,2,3])).
%% ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0061: function call 'lists:member' is not supported in preprocessor conditions
-define(FOO, 1).
-endif.

foo() -> ok.
"#,
        );
    }
}
