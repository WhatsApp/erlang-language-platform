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
use crate::lazy_function_matches;

pub(crate) struct NoSizeLinter;

impl Linter for NoSizeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoSize
    }
    fn description(&self) -> String {
        "Avoid using the `size/1` BIF.".to_string()
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl FunctionCallLinter for NoSizeLinter {
    type Context = ();

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::mfa("erlang", "size", 1)]]
    }
}

pub static LINTER: NoSizeLinter = NoSizeLinter;

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
