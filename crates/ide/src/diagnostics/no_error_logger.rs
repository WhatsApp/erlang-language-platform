/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::codemod_helpers::FunctionMatch;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;

pub(crate) struct NoErrorLoggerLinter;

impl Linter for NoErrorLoggerLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoErrorLogger
    }
    fn description(&self) -> &'static str {
        "The `error_logger` module is deprecated."
    }
    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl FunctionCallLinter for NoErrorLoggerLinter {
    type Context = ();

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        crate::lazy_function_matches![vec![FunctionMatch::m("error_logger")]]
    }
}

pub static LINTER: NoErrorLoggerLinter = NoErrorLoggerLinter;

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;

    #[test]
    fn no_error_logger() {
        check_diagnostics(
            r#"
              //- /src/main.erl
              -module(main).
              foo() -> error_logger:error_msg("ops").
                    %% ^^^^^^^^^^^^^^^^^^^^^^ 💡 error: The `error_logger` module is deprecated.
              //- /src/error_logger.erl
              -module(error_logger).
              -export([error_msg/1]).
              error_msg(_Msg) -> ok.
             "#,
        )
    }

    #[test]
    fn no_error_logger_imported() {
        check_diagnostics(
            r#"
              //- /src/main.erl
              -module(main).
              -import(error_logger, [error_msg/1]).
              foo() -> error_msg("ops").
                    %% ^^^^^^^^^^^^^^^^ 💡 error: The `error_logger` module is deprecated.
              //- /src/error_logger.erl
              -module(error_logger).
              -export([error_msg/1]).
              error_msg(_Msg) -> ok.
             "#,
        )
    }
}
