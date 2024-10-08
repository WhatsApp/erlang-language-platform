/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: undefined-function
//
// Return a warning when invoking a function which has no known definition.
// This functionality is similar to the one provided by the XRef tool which comes with OTP,
// but relies on the internal ELP database.
// Only fully qualified calls are reported by this diagnostic (e.g. `foo:bar/2`), since
// calls to undefined local functions are already reported by the Erlang linter itself (L1227).

use elp_ide_db::elp_base_db::FileId;
use hir::known;
use hir::Expr;
use hir::FunctionDef;
use hir::Semantic;
use text_edit::TextRange;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MakeDiagCtx;
use crate::FunctionMatch;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: true,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        undefined_function(diags, sema, file_id);
    },
};

fn undefined_function(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.def_map(file_id)
        .get_functions()
        .for_each(|(_arity, def)| {
            if def.file.file_id == file_id {
                check_function(diagnostics, sema, def)
            }
        });
}

fn check_function(diags: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    let matcher = FunctionMatch::any();
    find_call_in_function(
        diags,
        sema,
        def,
        &[(&matcher, ())],
        &move |CheckCallCtx {
                   target,
                   args,
                   in_clause: def_fb,
                   ..
               }: CheckCallCtx<'_, ()>| {
            let arity = args.len() as u32;
            match target {
                hir::CallTarget::Remote { module, name, .. } => {
                    let module = &def_fb[*module];
                    let name = &def_fb[*name];
                    if in_exclusion_list(sema, module, name, arity) {
                        None
                    } else {
                        match target.resolve_call(arity, sema, def_fb.file_id(), &def_fb.body()) {
                            Some(_) => None,
                            None => target
                                .label(arity, sema, &def_fb.body())
                                .map(|label| (label.to_string(), "".to_string())),
                        }
                    }
                }
                // Diagnostic L1227 already covers the case for local calls, so avoid double-reporting
                hir::CallTarget::Local { .. } => None,
            }
        },
        &move |ctx @ MakeDiagCtx { sema, extra, .. }| {
            let diag = make_diagnostic(sema, def.file.file_id, ctx.range_mf_only(), &extra.0);
            Some(diag)
        },
    );
}

fn in_exclusion_list(sema: &Semantic, module: &Expr, function: &Expr, arity: u32) -> bool {
    sema.is_atom_named(function, known::module_info) && (arity == 0 || arity == 1)
        || sema.is_atom_named(module, known::graphql_scanner)
        || sema.is_atom_named(module, known::graphql_parser)
        || sema.is_atom_named(module, known::thrift_scanner)
        || sema.is_atom_named(module, known::thrift_parser)
}

fn make_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    range: TextRange,
    function_name: &str,
) -> Diagnostic {
    let message = format!("Function '{}' is undefined.", function_name);
    Diagnostic::new(DiagnosticCode::UndefinedFunction, message, range)
        .with_severity(Severity::Warning)
        .with_ignore_fix(sema, file_id)
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_local() {
        check_diagnostics(
            r#"
  -module(main).
  main() ->
    exists(),
    not_exists().

  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn test_bif() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main(A) ->
    size(A),
    exists().

  exists() -> ok.
//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
    -module(erlang).
    -export([size/1]).
    size(_) -> ok.
            "#,
        )
    }

    #[test]
    fn test_erlang_typo() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    _T0 = erlang:monotonic_time(milliseconds),
    _T2 = erlang:monitonic_time(milliseconds),
%%        ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'erlang:monitonic_time/1' is undefined.
    exists().

  exists() -> ok.
//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
    -module(erlang).
    -export([monotonic_time/1]).
    monotonic_time(_) -> ok.
            "#,
        )
    }

    #[test]
    fn test_remote() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    dependency:exists(),
    dependency:not_exists().
%%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'dependency:not_exists/0' is undefined.
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn test_exclude_module_info() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    dependency:exists(),
    dependency:module_info().
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn test_exclude_module_info_different_arity() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    dependency:exists(),
    dependency:module_info(a, b).
%%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'dependency:module_info/2' is undefined.
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn test_do_not_exclude_get_stacktrace() {
        // erlang:get_stacktrace/0 last existed in OTP20. Do not special-case it
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    erlang:get_stacktrace(),
%%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'erlang:get_stacktrace/0' is undefined.
    dependency:get_stacktrace().
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'dependency:get_stacktrace/0' is undefined.
            "#,
        )
    }

    #[test]
    fn test_in_macro() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  -define(MY_MACRO, fun() -> dep:exists(), dep:not_exists() end).
  main() ->
    ?MY_MACRO().
%%  ^^^^^^^^^^^ 💡 warning: Function 'dep:not_exists/0' is undefined.
  exists() -> ok.
//- /src/dep.erl
  -module(dep).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn module_macro() {
        // https://github.com/WhatsApp/erlang-language-platform/issues/23
        check_diagnostics(
            r#"
            //- /src/elp_w0017.erl
            -module(elp_w0017).
            -export([loop/0]).

            loop() ->
              timer:sleep(1000),
              ?MODULE:loop().

            //- /src/timer.erl
            -module(timer).
            -export([sleep/1]).
            sleep(X) -> X.
            "#,
        )
    }

    #[test]
    fn test_ignore_fix() {
        check_fix(
            r#"
//- /src/main.erl
-module(main).

main() ->
  dep:exists(),
  dep:not_ex~ists().

exists() -> ok.
//- /src/dep.erl
-module(dep).
-compile(export_all).
exists() -> ok.
"#,
            expect![[r#"
-module(main).

main() ->
  dep:exists(),
  % elp:ignore W0017 (undefined_function)
  dep:not_exists().

exists() -> ok.
"#]],
        )
    }
}
