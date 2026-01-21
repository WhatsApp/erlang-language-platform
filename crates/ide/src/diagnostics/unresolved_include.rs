/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Diagnostic: unresolved-include (HIR)
//!
//! Reports include/include_lib files that cannot be resolved during HIR lowering.

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::TextRange;
use hir::BodyDiagnostic;
use hir::IncludeAttribute;
use hir::Semantic;

use super::collect_body_diagnostics;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct UnresolvedIncludeLinter;

/// Context stores pre-computed message since match_description has no db access
#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) struct UnresolvedIncludeContext {
    pub message: String,
}

impl Linter for UnresolvedIncludeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::HirUnresolvedInclude
    }

    fn description(&self) -> &'static str {
        "unresolved include"
    }

    fn is_enabled(&self) -> bool {
        false
    }
}

impl GenericLinter for UnresolvedIncludeLinter {
    type Context = UnresolvedIncludeContext;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let body_diagnostics = collect_body_diagnostics(sema, file_id);
        let matches: Vec<_> = body_diagnostics
            .into_iter()
            .filter_map(|diag| {
                if let BodyDiagnostic::UnresolvedInclude(include) = diag {
                    // Only include diagnostics for the requested file
                    if include.file_id != file_id {
                        return None;
                    }

                    // Get the include attribute from the form_list
                    let form_list = sema.form_list(file_id);
                    let include_attr = &form_list[include.value];

                    // Extract path and range from IncludeAttribute
                    let path = include_attr.path().to_string();
                    let range: TextRange = include_attr.file_range(sema.db, file_id);

                    // Use appropriate message based on include type
                    let message = match include_attr {
                        IncludeAttribute::Include { .. } => {
                            format!("can't find include file \"{}\"", path)
                        }
                        IncludeAttribute::IncludeLib { .. } => {
                            format!("can't find include lib \"{}\"", path)
                        }
                    };

                    Some(GenericLinterMatchContext {
                        range,
                        context: UnresolvedIncludeContext { message },
                    })
                } else {
                    None
                }
            })
            .collect();
        Some(matches)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(context.message.clone())
    }
}

pub static LINTER: UnresolvedIncludeLinter = UnresolvedIncludeLinter;

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn unresolved_include_basic() {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::HirUnresolvedInclude);
        check_diagnostics_with_config(
            config,
            r#"
            -module(main).
            -include("nonexistent.hrl").
            %%       ^^^^^^^^^^^^^^^^^ 💡 warning: W0058: can't find include file "nonexistent.hrl"
            "#,
        );
    }

    #[test]
    fn unresolved_include_lib() {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::HirUnresolvedInclude);
        check_diagnostics_with_config(
            config,
            r#"
            -module(main).
            -include_lib("some_app/include/nonexistent.hrl").
            %%           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0058: can't find include lib "some_app/include/nonexistent.hrl"
            "#,
        );
    }
}
