/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: undefined-function
//
// Return a warning when invoking a function which has no known definition.
// This functionality is similar to the one provided by the XRef tool which comes with OTP,
// but relies on the internal ELP database.
// Only fully qualified calls are reported by this diagnostic (e.g. `foo:bar/2`), since
// calls to undefined local functions are already reported by the Erlang linter itself (L1227).

use std::borrow::Cow;

use elp_syntax::SmolStr;
use hir::Expr;
use hir::Module;
use hir::Semantic;
use hir::known;

use super::DiagnosticCode;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::lazy_function_matches;

pub(crate) struct UndefinedFunctionLinter;

impl Linter for UndefinedFunctionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UndefinedFunction
    }
    fn description(&self) -> &'static str {
        "Function is undefined."
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
    // Ideally, we would like to report undefined functions in all files, but
    // there are too many false positives in test files to do so.
    // This is often due to mocked modules and test suite cleverness.
    // We can revisit this decision in the future. See T249044930.
    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl FunctionCallLinter for UndefinedFunctionLinter {
    type Context = Option<SmolStr>;

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::any()]]
    }

    fn excludes_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![
            FunctionMatch::m("lager"), // Lager functions are produced by parse transforms
        ]]
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        match context {
            None => Cow::Borrowed(self.description()),
            Some(function_name) => Cow::Owned(format!("Function '{function_name}' is undefined.")),
        }
    }

    fn check_match(&self, context: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        match context.target {
            hir::CallTarget::Remote { module, name, .. } => {
                let sema = context.in_clause.sema;
                let def_fb = context.in_clause;
                let arity = context.args.arity();
                let module = &def_fb[*module];
                let name = &def_fb[*name];

                // If the module is a variable, we can't determine at compile time
                // whether the function is defined or not, so don't report it
                if module.as_var().is_some() || name.as_var().is_some() {
                    return None;
                }

                if sema.is_atom_named(name, &known::module_info) && (arity == 0 || arity == 1) {
                    return None;
                }

                // Try to resolve the module expression to a static module
                let resolved_module = sema.resolve_module_expr(def_fb.file_id(), module);

                match resolved_module {
                    Some(resolved_module) => {
                        // Module exists, check if the function exists
                        match context.target.resolve_call(
                            arity,
                            sema,
                            def_fb.file_id(),
                            &def_fb.body(),
                        ) {
                            Some(_) => None,
                            None => {
                                // Function doesn't exist - check if it would be automatically added
                                if is_automatically_added(sema, resolved_module, name, arity) {
                                    return None;
                                }
                                Some(context.target.label(arity, sema, &def_fb.body()))
                            }
                        }
                    }
                    None => {
                        // Module cannot be resolved. If the module expression is a static atom,
                        // it means the module doesn't exist and we should report it.
                        // If it's a dynamic expression (e.g., a variable or function call),
                        // we can't determine at compile time whether it's defined.
                        if module.as_atom().is_some() {
                            // Static module name that doesn't exist - report as undefined
                            Some(context.target.label(arity, sema, &def_fb.body()))
                        } else {
                            // Dynamic module expression - can't determine at compile time
                            None
                        }
                    }
                }
            }
            // Diagnostic L1227 already covers the case for local calls, so avoid double-reporting
            hir::CallTarget::Local { .. } => None,
        }
    }
}

pub static LINTER: UndefinedFunctionLinter = UndefinedFunctionLinter;

fn is_automatically_added(sema: &Semantic, module: Module, function: &Expr, arity: u32) -> bool {
    // If the module defines callbacks, {behaviour,behavior}_info are automatically defined
    let module_has_callbacks_defined: bool = sema
        .form_list(module.file.file_id)
        .callback_attributes()
        .next()
        .is_some();

    let function_name_is_behaviour_info: bool = sema
        .is_atom_named(function, &known::behaviour_info)
        || sema.is_atom_named(function, &known::behavior_info);

    function_name_is_behaviour_info && arity == 1 && module_has_callbacks_defined
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().disable(elp_ide_db::DiagnosticCode::NoSize);
        check_diagnostics_with_config(config, fixture)
    }

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
%%        ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'erlang:monitonic_time/1' is undefined.
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
%%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'dependency:not_exists/0' is undefined.
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn test_callbacks_define_behaviour_info() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  -callback foo() -> ok.
  main() ->
    ?MODULE:behaviour_info(callbacks),
    ?MODULE:behavior_info(callbacks),
    main:behaviour_info(callbacks),
    hascallback:behaviour_info(callbacks),
    nocallback:behaviour_info(callbacks),
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'nocallback:behaviour_info/1' is undefined.
    nonexisting:behaviour_info(callbacks),
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'nonexisting:behaviour_info/1' is undefined.

    behaviour_info(callbacks).
//- /src/hascallback.erl
   -module(hascallback).
   -callback foo() -> ok.
   go() -> ok.
//- /src/nocallback.erl
   -module(nocallback).
   go() -> ok.
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
%%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'dependency:module_info/2' is undefined.
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
%%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'erlang:get_stacktrace/0' is undefined.
    dependency:get_stacktrace().
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'dependency:get_stacktrace/0' is undefined.
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
%%  ^^^^^^^^^^^ 💡 warning: W0017: Function 'dep:not_exists/0' is undefined.
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
              my_timer:sleep(1_000),
              ?MODULE:loop().

            //- /src/my_timer.erl
            -module(my_timer).
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

    #[test]
    fn test_capture_fun() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    {fun dependency:exists/0,
    fun dependency:not_exists/1,
%%      ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'dependency:not_exists/1' is undefined.
    fun dependency:module_info/2}.
%%      ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0017: Function 'dependency:module_info/2' is undefined.
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn test_exclusion_list() {
        check_diagnostics(
            r#"
    //- /src/main.erl
    -module(main).
    main() ->
      lager:warning("Some ~p", [Message]).
            "#,
        )
    }

    #[test]
    fn test_module_name_variable() {
        check_diagnostics(
            r#"
    //- /src/main.erl
    -module(main).
    main(Callback) ->
      Callback:main().
            "#,
        )
    }

    #[test]
    fn test_function_name_variable() {
        check_diagnostics(
            r#"
    //- /src/main.erl
    -module(main).
    main(Callback) ->
      main:Callback().
            "#,
        )
    }

    #[test]
    fn test_macro_remote_call() {
        check_diagnostics(
            r#"
    //- /src/main.erl
    -module(main).
    -export([do/0]).
    -define(COUNT_BACKEND, (count_backend:count_module())).
    -define(COUNT(Name), ?COUNT_BACKEND:count(Name)).
    
    do() ->
        ?COUNT('error.count').
    
    //- /src/stats.erl
    -module(stats).
    -export([count/1]).
    count(_) -> ok.
    //- /src/count_backend.erl
    -module(count_backend).
    -export([count_module/0]).
    count_module() -> ?MODULE.
            "#,
        )
    }
}
