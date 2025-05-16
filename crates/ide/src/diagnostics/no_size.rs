/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use lazy_static::lazy_static;

use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::helpers::DiagnosticTemplate;
use super::helpers::check_used_functions;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::UseRange;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;
use crate::diagnostics::helpers::FunctionCallDiagnostic;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false,
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
            code: DiagnosticCode::NoSize,
            message: "Avoid using the `size/1` BIF.".to_string(),
            severity: Severity::Warning,
            with_ignore_fix: true,
            use_range: UseRange::NameOnly,
        },
        matches: vec![FunctionMatch::mfa("erlang", "size", 1)]
            .into_iter()
            .collect(),
    }]
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn basic() {
        check_diagnostics(
            r#"
              //- /src/main.erl
              -module(main).
              foo() -> erlang:size({}).
              %%       ^^^^^^^^^^^ 💡 warning: Avoid using the `size/1` BIF.
              
              bar() -> size(<<>>).
              %%       ^^^^ 💡 warning: Avoid using the `size/1` BIF.
              //- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
              -module(erlang).
              -export([size/1]).
              size(_) -> 42.
             "#,
        )
    }

    #[test]
    fn basic_fix_ignore() {
        check_fix(
            r#"
              //- /src/main.erl
              -module(main).
              foo() -> er~lang:size({}).
              
              bar() -> size(<<>>).
              //- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
              -module(erlang).
              -export([size/1]).
              size(_) -> 42.
             "#,
            expect![[r#"
                -module(main).
                % elp:ignore W0050 (no_size)
                foo() -> erlang:size({}).

                bar() -> size(<<>>).
            "#]],
        );
    }
}
