/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use lazy_static::lazy_static;

use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::UseRange;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::diagnostics::helpers::DiagnosticTemplate;
use crate::diagnostics::helpers::FunctionCallDiagnostic;
use crate::diagnostics::helpers::check_used_functions;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        check_used_functions(sema, file_id, &USED_FUNCTIONS, diags);
    },
};

lazy_static! {
    static ref USED_FUNCTIONS: Vec<FunctionCallDiagnostic> = make_static_used_functions();
}

fn make_static_used_functions() -> Vec<FunctionCallDiagnostic> {
    vec![FunctionCallDiagnostic {
        diagnostic_template: DiagnosticTemplate {
            code: DiagnosticCode::NoErrorLogger,
            message: "The `error_logger` module is deprecated.".to_string(),
            severity: Severity::Error,
            with_ignore_fix: true,
            use_range: UseRange::NameOnly,
        },
        matches: vec![FunctionMatch::m("error_logger")],
    }]
}

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
