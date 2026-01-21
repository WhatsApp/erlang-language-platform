/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Diagnostic: unresolved-macro (HIR)
//!
//! Reports macros that cannot be resolved during HIR lowering.

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use hir::BodyDiagnostic;
use hir::Semantic;

use super::collect_body_diagnostics;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct UnresolvedMacroLinter;

/// Context stores pre-computed message since match_description has no db access
#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) struct UnresolvedMacroContext {
    pub message: String,
}

impl Linter for UnresolvedMacroLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::HirUnresolvedMacro
    }

    fn description(&self) -> &'static str {
        "undefined macro"
    }

    fn is_enabled(&self) -> bool {
        false
    }
}

impl GenericLinter for UnresolvedMacroLinter {
    type Context = UnresolvedMacroContext;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let body_diagnostics = collect_body_diagnostics(sema, file_id);
        let matches: Vec<_> = body_diagnostics
            .into_iter()
            .filter_map(|diag| {
                if let BodyDiagnostic::UnresolvedMacro(macro_source) = diag {
                    // Only include diagnostics for the requested file
                    if macro_source.file_id() != file_id {
                        return None;
                    }

                    // Get the macro call AST node to extract name and arity
                    let full_range = macro_source.range();
                    let macro_call = macro_source.to_ast(sema.db);
                    let macro_name = macro_call
                        .name()
                        .map(|name| name.to_string())
                        .unwrap_or_else(|| "?".to_string());

                    let message = match macro_call.arity() {
                        Some(arity) => format!("undefined macro '{}/{}'", macro_name, arity),
                        None => format!("undefined macro '{}'", macro_name),
                    };

                    // For macros with arguments, only highlight the name part, not the full call
                    let range = macro_call
                        .name()
                        .map(|name| {
                            // Get the syntax range of just the macro name
                            let name_range = name.syntax().text_range();
                            // Include the '?' prefix by extending one character to the left
                            if name_range.start() > 0.into() {
                                TextRange::new(
                                    name_range.start() - TextSize::from(1),
                                    name_range.end(),
                                )
                            } else {
                                name_range
                            }
                        })
                        .unwrap_or(full_range.range);

                    Some(GenericLinterMatchContext {
                        range,
                        context: UnresolvedMacroContext { message },
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

pub static LINTER: UnresolvedMacroLinter = UnresolvedMacroLinter;

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn unresolved_macro_basic() {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirUnresolvedMacro)
            .disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(
            config,
            r#"
            -module(main).
            foo() -> ?UNDEFINED_MACRO.
            %%       ^^^^^^^^^^^^^^^^ 💡 warning: W0057: undefined macro 'UNDEFINED_MACRO'
            "#,
        );
    }

    #[test]
    fn unresolved_macro_with_arity() {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::HirUnresolvedMacro)
            .disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(
            config,
            r#"
            -module(main).
            foo(X) -> ?UNDEFINED_MACRO(X, Y).
            %%        ^^^^^^^^^^^^^^^^ 💡 warning: W0057: undefined macro 'UNDEFINED_MACRO/2'
            "#,
        );
    }
}
