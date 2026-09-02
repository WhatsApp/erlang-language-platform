/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Diagnostic for injecting failures to test CI integration.
//!
//! Matches calls to the (otherwise non-existent) function
//! `elp_ci:provoke_diagnostic(Message, Level)`. The first argument is a
//! string literal that is included in the resulting diagnostic, and the
//! second argument is an atom selecting the behaviour:
//!
//! - `panic`   — panic from inside the linter, exercising the CI panic path
//! - `error`   — emit a diagnostic at error severity
//! - `warning` — emit a diagnostic at warning severity
//! - `notice`  — emit a diagnostic at weak-warning (notice) severity

use std::borrow::Cow;

use hir::Expr;
use hir::Literal;

use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::Severity;
use crate::lazy_function_matches;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub(crate) enum ProvokeLevel {
    Error,
    #[default]
    Warning,
    Notice,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub(crate) struct ProvokeContext {
    pub message: String,
    pub level: ProvokeLevel,
}

pub(crate) struct ElpCiTestDiagnosticLinter;

impl Linter for ElpCiTestDiagnosticLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::ElpCiTestDiagnostic
    }

    fn description(&self) -> &'static str {
        "Synthetic diagnostic injected via elp_ci:provoke_diagnostic/2 for CI testing."
    }

    fn should_process_test_files(&self) -> bool {
        true
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }
}

impl FunctionCallLinter for ElpCiTestDiagnosticLinter {
    type Context = ProvokeContext;

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::mfa("elp_ci", "provoke_diagnostic", 2,)]]
    }

    fn check_match(&self, ctx: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        let message = literal_string(ctx, 0).unwrap_or_else(|| "<missing message>".to_string());
        let level_atom = literal_atom(ctx, 1);

        match level_atom.as_deref() {
            Some("panic") => {
                panic!("elp_ci:provoke_diagnostic panic injected for CI testing: {message}");
            }
            Some("error") => Some(ProvokeContext {
                message,
                level: ProvokeLevel::Error,
            }),
            Some("warning") => Some(ProvokeContext {
                message,
                level: ProvokeLevel::Warning,
            }),
            Some("notice") => Some(ProvokeContext {
                message,
                level: ProvokeLevel::Notice,
            }),
            _ => Some(ProvokeContext {
                message,
                level: ProvokeLevel::Warning,
            }),
        }
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "CI test diagnostic ({:?}): {}",
            context.level, context.message
        ))
    }

    fn match_severity(&self, context: &Self::Context, _ctx: &LinterContext) -> Severity {
        match context.level {
            ProvokeLevel::Error => Severity::Error,
            ProvokeLevel::Warning => Severity::Warning,
            ProvokeLevel::Notice => Severity::WeakWarning,
        }
    }
}

fn literal_string(ctx: &CheckCallCtx<'_, ()>, idx: usize) -> Option<String> {
    let expr_id = ctx.args.get(idx)?;
    if let Expr::Literal(Literal::String(s)) = &ctx.in_clause[expr_id] {
        Some(s.as_string())
    } else {
        None
    }
}

fn literal_atom(ctx: &CheckCallCtx<'_, ()>, idx: usize) -> Option<String> {
    let expr_id = ctx.args.get(idx)?;
    if let Expr::Literal(Literal::Atom(atom)) = &ctx.in_clause[expr_id] {
        Some(atom.as_string())
    } else {
        None
    }
}

pub static LINTER: ElpCiTestDiagnosticLinter = ElpCiTestDiagnosticLinter;

#[cfg(test)]
mod tests {

    use crate::DiagnosticsConfig;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn test_provoke_warning() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction),
            r#"
//- /src/main.erl
-module(main).
-export([go/0]).

go() ->
    elp_ci:provoke_diagnostic("hello from CI", warning).
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0079: CI test diagnostic (Warning): hello from CI
%%                          | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn test_provoke_error() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction),
            r#"
//- /src/main.erl
-module(main).
-export([go/0]).

go() ->
    elp_ci:provoke_diagnostic("boom", error).
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^ error: W0079: CI test diagnostic (Error): boom
%%                          | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn test_provoke_notice() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction),
            r#"
//- /src/main.erl
-module(main).
-export([go/0]).

go() ->
    elp_ci:provoke_diagnostic("fyi", notice).
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0079: CI test diagnostic (Notice): fyi
%%                          | 💡 <suppression>
            "#,
        );
    }

    #[test]
    #[should_panic(expected = "elp_ci:provoke_diagnostic panic injected for CI testing")]
    fn test_provoke_panic() {
        tests::check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-export([go/0]).

go() ->
    elp_ci:provoke_diagnostic("kaboom", panic).
            "#,
        );
    }
}
