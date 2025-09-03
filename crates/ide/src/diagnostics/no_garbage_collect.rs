/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::FunctionMatch;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::lazy_function_matches;

pub(crate) struct NoGarbageCollectLinter;

impl Linter for NoGarbageCollectLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoGarbageCollect
    }
    fn description(&self) -> String {
        "Avoid forcing garbage collection.".to_string()
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl FunctionCallLinter for NoGarbageCollectLinter {
    type Context = ();

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::mf("erlang", "garbage_collect")]]
    }
}

pub static LINTER: NoGarbageCollectLinter = NoGarbageCollectLinter;

#[cfg(test)]
mod tests {

    use crate::tests;

    #[test]
    fn test_erlang_garbage_collect_usage() {
        tests::check_diagnostics(
            r#"
  //- /src/main.erl
  -module(main).
  -export([error/0]).
  
  error() ->
      erlang:garbage_collect().
  %%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Avoid forcing garbage collection.
  //- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
  -module(erlang).
  -export([garbage_collect/0]).
  garbage_collect() -> ok.
              "#,
        )
    }

    #[test]
    fn test_collect_usage() {
        tests::check_diagnostics(
            r#"
  //- /src/main.erl
  -module(main).
  -export([error/0]).
  
  error() ->
      garbage_collect().
  %%  ^^^^^^^^^^^^^^^ 💡 warning: Avoid forcing garbage collection.
  //- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
  -module(erlang).
  -export([garbage_collect/0]).
  garbage_collect() -> ok.
              "#,
        )
    }
}
