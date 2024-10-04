/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: slow_functions
//!
//! Return a diagnostic if a slow function is used.

use elp_ide_db::elp_base_db::FileId;
use hir::Semantic;
use lazy_static::lazy_static;

use super::helpers::check_used_functions;
use super::helpers::DiagnosticTemplate;
use super::Diagnostic;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::UseRange;
use crate::diagnostics::helpers::FunctionCallDiagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        slow_functions(diags, sema, file_id);
    },
};

fn slow_functions(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    check_used_functions(sema, file_id, &USED_FUNCTIONS, diags);
}

lazy_static! {
    static ref USED_FUNCTIONS: Vec<FunctionCallDiagnostic> = make_static_used_functions();
}

fn make_static_used_functions() -> Vec<FunctionCallDiagnostic> {
    vec![
        FunctionCallDiagnostic {
            diagnostic_template: DiagnosticTemplate {
                code: DiagnosticCode::SlowFunction,
                message: r#"Prefer the map-based sets implementation.
The old implementation of sets was very slow. Use `[{version, 2}]` when constructing a set.

See https://www.erlang.org/doc/man/sets.html
"#
                .to_string(),
                severity: Severity::Warning,
                with_ignore_fix: true,
                use_range: UseRange::NameOnly
            },
            matches: vec![
                FunctionMatch::mfas("sets", "new", vec![0]),
                FunctionMatch::mfas("sets", "from_list", vec![1]),
            ]
            .into_iter()
            .flatten()
            .collect(),
        },
        FunctionCallDiagnostic {
            diagnostic_template: DiagnosticTemplate {
                code: DiagnosticCode::SlowFunction,
                message: format!("The dict module is very slow in Erlang.\nIt is recommend to use maps instead of the dict module.\n\n{}\n",
                                 // @fb-only
                                 "" // @oss-only
                                 )
                .to_string(),
                severity: Severity::Warning,
                with_ignore_fix: true,
                use_range: UseRange::NameOnly
            },
            matches: vec![FunctionMatch::m("dict")],
        },
    ]
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
             %%       ^^^^^^^^ 💡 warning: Prefer the map-based sets implementation.
             %%              |The old implementation of sets was very slow. Use `[{version, 2}]` when constructing a set.
             %%              | 
             %%              |See https://www.erlang.org/doc/man/sets.html
             %%              | 

             bar() -> sets:from_list([]).
             %%       ^^^^^^^^^^^^^^ 💡 warning: Prefer the map-based sets implementation.
             %%                    |The old implementation of sets was very slow. Use `[{version, 2}]` when constructing a set.
             %%                    | 
             %%                    |See https://www.erlang.org/doc/man/sets.html
             %%                    | 

             //- /src/sets.erl
             -module(sets).
             -export([new/0, from_list/1]).
             new() -> ok.
             from_list(_) -> ok.
            "#,
        )
    }

    // @fb-only
    // @oss-only #[allow(dead_code)]
    fn slow_function_dict() {
        check_diagnostics(
            r#"
             //- /src/main.erl
             -module(main).
             foo() -> dict:new().
             %%       ^^^^^^^^ 💡 warning: The dict module is very slow in Erlang. 
             %%              |It is recommend to use maps instead of the dict module.
             %%              | 
             %%              |See https://fb.workplace.com/groups/1178411125511220/permalink/3495040030514973
             %%              | 

             bar() -> dict:from_list([]).
             %%       ^^^^^^^^^^^^^^ 💡 warning: The dict module is very slow in Erlang. 
             %%                    |It is recommend to use maps instead of the dict module.
             %%                    | 
             %%                    |See https://fb.workplace.com/groups/1178411125511220/permalink/3495040030514973
             %%                    | 

             //- /src/dict.erl
             -module(dict).
             -export([new/0, from_list/1]).
             new() -> ok.
             from_list(_) -> ok.
            "#,
        )
    }
}
