/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FileId;
use hir::FunctionDef;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::find_call_in_function;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::NoGarbageCollect;
const DIAGNOSTIC_MESSAGE: &str = "Avoid forcing garbage collection.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::Warning;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        no_garbage_collect(diags, sema, file_id);
    },
};

fn no_garbage_collect(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    lazy_static! {
        static ref BAD_CALLS: Vec<FunctionMatch> =
            vec![FunctionMatch::mf("erlang", "garbage_collect")];
        static ref BAD_CALLS_MFAS: Vec<(&'static FunctionMatch, ())> = BAD_CALLS
            .iter()
            .map(|matcher| (matcher, ()))
            .collect::<Vec<_>>();
    }

    sema.def_map_local(file_id)
        .get_functions()
        .for_each(|(_arity, def)| {
            check_function(diagnostics, sema, def, &BAD_CALLS_MFAS);
        });
}

fn check_function(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    mfas: &[(&FunctionMatch, ())],
) {
    find_call_in_function(
        diags,
        sema,
        def,
        mfas,
        &move |CheckCallCtx { .. }: CheckCallCtx<'_, ()>| Some(()),
        &move |ctx @ MatchCtx { .. }| {
            let range = ctx.range_mf_or_macro();
            let diagnostic = Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, range.range)
                .with_ignore_fix(sema, def.file.file_id)
                .with_severity(DIAGNOSTIC_SEVERITY);
            Some(diagnostic)
        },
    );
}

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
