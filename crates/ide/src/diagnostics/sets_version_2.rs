/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_project_model::otp;

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
    fn description(&self) -> &'static str {
        "Prefer `[{version, 2}]` when constructing a set."
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
    fn is_enabled(&self) -> bool {
        otp::sets_v2_not_default()
    }
}

impl FunctionCallLinter for SetsVersion2Linter {
    type Context = ();

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

    use elp_project_model::otp;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::SetsVersion2
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        if otp::sets_v2_not_default() {
            tests::check_filtered_diagnostics(fixture, &filter)
        }
    }

    #[test]
    fn slow_function_sets() {
        check_diagnostics(
            r#"
             //- /src/main.erl
             -module(main).
             foo() -> sets:new().
             %%       ^^^^^^^^ 💡 warning: W0049: Prefer `[{version, 2}]` when constructing a set.

             bar() -> sets:from_list([]).
             %%       ^^^^^^^^^^^^^^ 💡 warning: W0049: Prefer `[{version, 2}]` when constructing a set.

             //- /src/sets.erl
             -module(sets).
             -export([new/0, from_list/1]).
             new() -> ok.
             from_list(_) -> ok.
            "#,
        )
    }
}
