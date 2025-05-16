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
            code: DiagnosticCode::SetsVersion2,
            message: "Prefer `[{version, 2}]` when constructing a set.".to_string(),
            severity: Severity::Warning,
            with_ignore_fix: true,
            use_range: UseRange::NameOnly,
        },
        matches: vec![
            FunctionMatch::mfas("sets", "new", vec![0]),
            FunctionMatch::mfas("sets", "from_list", vec![1]),
        ]
        .into_iter()
        .flatten()
        .collect(),
    }]
}

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
