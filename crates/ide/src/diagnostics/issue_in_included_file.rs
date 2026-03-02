/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Diagnostic: issue-in-included-file (W0062)
//!
//! Reports issues in included files, with the diagnostic shown at the
//! include directive and related info pointing to the actual issue.

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::TextRange;
use hir::InFile;
use hir::PPCondition;
use hir::Semantic;

use super::compute_pp_condition_range;
use super::find_include_directive_for_file;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::RelatedInformation;

pub(crate) struct IssueInIncludedFileLinter;

/// Context stores information needed for the diagnostic and related info
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct IssueInIncludedFileContext {
    pub message: String,
    pub included_file_id: FileId,
    pub condition_range: TextRange,
}

impl Linter for IssueInIncludedFileLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::HirIssueInIncludedFile
    }

    fn description(&self) -> &'static str {
        "issue in included file"
    }

    fn is_enabled(&self) -> bool {
        false
    }
}

impl GenericLinter for IssueInIncludedFileLinter {
    type Context = IssueInIncludedFileContext;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let form_list = sema.form_list(file_id);
        let mut matches = Vec::new();

        // Scan included files for PP condition issues
        for (include_id, _include) in form_list.includes() {
            if let Some(included_file_id) = sema.db.resolve_include(
                sema.db.app_data_id_by_file(file_id),
                InFile::new(file_id, include_id),
            ) {
                // Get all condition diagnostics from the preprocessor analysis for the included file
                let env = sema.db.project_macro_environment(included_file_id);
                let (_, diagnostics_map) = sema
                    .db
                    .file_preprocessor_analysis_with_diagnostics(included_file_id, env);

                // Check the included file for PP condition diagnostics
                let included_form_list = sema.form_list(included_file_id);

                for (cond_id, condition) in included_form_list.pp_conditions() {
                    // Only -if and -elif conditions can generate diagnostics
                    let has_expr =
                        matches!(condition, PPCondition::If { .. } | PPCondition::Elif { .. });

                    if has_expr {
                        // Get diagnostics from the map
                        if let Some(cond_diagnostics) = diagnostics_map.get(&cond_id) {
                            for cond_diag in cond_diagnostics {
                                // Find the include directive range in the parent file
                                if let Some((_include_id, include_range)) =
                                    find_include_directive_for_file(
                                        sema.db,
                                        file_id,
                                        included_file_id,
                                    )
                                {
                                    let condition_range = compute_pp_condition_range(
                                        sema.db,
                                        included_file_id,
                                        cond_id,
                                    );

                                    matches.push(GenericLinterMatchContext {
                                        range: include_range,
                                        context: IssueInIncludedFileContext {
                                            message: cond_diag.message.clone(),
                                            included_file_id,
                                            condition_range,
                                        },
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        Some(matches)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("issue in included file: {}", context.message))
    }

    fn related(
        &self,
        context: &Self::Context,
        _sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<RelatedInformation>> {
        Some(vec![RelatedInformation {
            file_id: context.included_file_id,
            range: context.condition_range,
            message: context.message.clone(),
        }])
    }
}

pub static LINTER: IssueInIncludedFileLinter = IssueInIncludedFileLinter;

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn invalid_pp_condition_in_included_file_w0062() {
        // Test W0062: invalid preprocessor condition in an included file
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirIssueInIncludedFile)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnspecificInclude);
        check_diagnostics_with_config(
            config,
            r#"
//- /include/header.hrl include_path:/include
-if(some_function()).
-define(FOO, 1).
-endif.

//- /src/main.erl
-module(main).
-include("header.hrl").
%%       ^^^^^^^^^^^^ 💡 warning: W0062: issue in included file: function call 'some_function' is not supported in preprocessor conditions
%%                    | Related info: 0:3-20 function call 'some_function' is not supported in preprocessor conditions

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_xor_in_included_file_w0062() {
        // Test W0062: xor operator in header file
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirIssueInIncludedFile)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnspecificInclude);
        check_diagnostics_with_config(
            config,
            r#"
//- /include/header.hrl include_path:/include
-if(true xor false).
-define(FOO, 1).
-endif.

//- /src/main.erl
-module(main).
-include("header.hrl").
%%       ^^^^^^^^^^^^ 💡 warning: W0062: issue in included file: the 'xor' operator is not supported in preprocessor conditions
%%                    | Related info: 0:3-19 the 'xor' operator is not supported in preprocessor conditions

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_undefined_macro_in_included_file_w0062() {
        // Test W0062: undefined macro in header file
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirIssueInIncludedFile)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnspecificInclude);
        check_diagnostics_with_config(
            config,
            r#"
//- /include/header.hrl include_path:/include
-if(?UNDEFINED_MACRO).
-define(FOO, 1).
-endif.

//- /src/main.erl
-module(main).
-include("header.hrl").
%%       ^^^^^^^^^^^^ 💡 warning: W0062: issue in included file: undefined macro 'UNDEFINED_MACRO' in preprocessor condition
%%                    | Related info: 0:3-21 undefined macro 'UNDEFINED_MACRO' in preprocessor condition

foo() -> ok.
"#,
        );
    }

    #[test]
    fn invalid_pp_condition_elif_in_included_file_w0062() {
        // Test W0062: invalid -elif in header file
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirIssueInIncludedFile)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnspecificInclude);
        check_diagnostics_with_config(
            config,
            r#"
//- /include/header.hrl include_path:/include
-ifdef(SOME_MACRO).
-define(FOO, 1).
-elif(some_function()).
-define(FOO, 2).
-endif.

//- /src/main.erl
-module(main).
-include("header.hrl").
%%       ^^^^^^^^^^^^ 💡 warning: W0062: issue in included file: function call 'some_function' is not supported in preprocessor conditions
%%                    | Related info: 0:42-59 function call 'some_function' is not supported in preprocessor conditions

foo() -> ok.
"#,
        );
    }

    #[test]
    fn no_issue_in_header_with_valid_condition() {
        // Test that valid conditions in header files do not produce W0062 diagnostics
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirIssueInIncludedFile)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnspecificInclude)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(
            config,
            r#"
//- /include/header.hrl include_path:/include
-ifdef(DEBUG).
-define(LOG(X), io:format("~p~n", [X])).
-else.
-define(LOG(X), ok).
-endif.

//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> ok.
"#,
        );
    }
}
