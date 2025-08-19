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

pub(crate) struct SetsVersion2Linter;

impl Linter for SetsVersion2Linter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::SetsVersion2
    }
    fn description(&self) -> String {
        "Prefer `[{version, 2}]` when constructing a set.".to_string()
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl FunctionCallLinter for SetsVersion2Linter {
    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![
            FunctionMatch::mfas("sets", "new", vec![0]),
            FunctionMatch::mfas("sets", "from_list", vec![1]),
        ]
    }
}

pub static LINTER: SetsVersion2Linter = SetsVersion2Linter;

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;

    #[test]
    fn slow_function_sets() {
        check_diagnostics(
            r#"
             //- /src/main.erl
             -module(main).
             foo() -> sets:new().
             %%       ^^^^^^^^ 💡 warning: Prefer `[{version, 2}]` when constructing a set.
             
             bar() -> sets:from_list([]).
             %%       ^^^^^^^^^^^^^^ 💡 warning: Prefer `[{version, 2}]` when constructing a set.
             
             //- /src/sets.erl
             -module(sets).
             -export([new/0, from_list/1]).
             new() -> ok.
             from_list(_) -> ok.
            "#,
        )
    }
}
