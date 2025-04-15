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

use elp_ide_assists::helpers;
use elp_ide_assists::helpers::ExportForm;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use hir::Expr;
use hir::FunctionDef;
use hir::Module;
use hir::NameArity;
use hir::Semantic;
use hir::known;
use text_edit::TextRange;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::find_call_in_function;
// @fb-only
use crate::fix;

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
    let check_for_unexported = true; // @oss-only
    // @fb-only
    sema.def_map_local(file_id)
        .get_functions()
        .for_each(|(_arity, def)| check_function(diagnostics, sema, def, check_for_unexported));
}

fn check_function(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    check_for_unexported: bool,
) {
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
            let arity = args.arity();
            match target {
                hir::CallTarget::Remote { module, name, .. } => {
                    let module = &def_fb[*module];
                    let name = &def_fb[*name];
                    if in_exclusion_list(sema, module, name, arity) {
                        None
                    } else if sema
                        .resolve_module_expr(def_fb.file_id(), module)
                        .is_some_and(|module| is_automatically_added(sema, module, name, arity))
                    {
                        None
                    } else {
                        let maybe_function_def =
                            target.resolve_call(arity, sema, def_fb.file_id(), &def_fb.body());
                        let function_exists = maybe_function_def.is_some();
                        let is_exported = maybe_function_def.clone().is_some_and(|fun_def| {
                            is_exported_function(fun_def.file.file_id, sema, &fun_def.name)
                        });

                        if function_exists && (is_exported) {
                            None
                        } else {
                            target.label(arity, sema, &def_fb.body()).map(|label| {
                                (
                                    label.to_string(),
                                    "".to_string(),
                                    function_exists && !is_exported,
                                    maybe_function_def,
                                )
                            })
                        }
                    }
                }
                // Diagnostic L1227 already covers the case for local calls, so avoid double-reporting
                hir::CallTarget::Local { .. } => None,
            }
        },
        &move |ctx @ MatchCtx { sema, extra, .. }| {
            make_diagnostic(
                sema,
                def.file.file_id,
                ctx.range_mf_only(),
                &extra.0,
                extra.2,
                extra.3.clone(),
                check_for_unexported,
            )
        },
    );
}

fn is_exported_function(file_id: FileId, sema: &Semantic, name: &NameArity) -> bool {
    sema.def_map(file_id).is_function_exported(name)
}

fn is_automatically_added(sema: &Semantic, module: Module, function: &Expr, arity: u32) -> bool {
    // If the module defines callbacks, {behaviour,behavior}_info are automatically defined
    let module_has_callbacks_defined: bool = sema
        .form_list(module.file.file_id)
        .callback_attributes()
        .next()
        .is_some();

    let function_name_is_behaviour_info: bool = sema.is_atom_named(function, known::behaviour_info)
        || sema.is_atom_named(function, known::behavior_info);

    function_name_is_behaviour_info && arity == 1 && module_has_callbacks_defined
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
    is_private: bool,
    maybe_function_def: Option<FunctionDef>,
    check_for_unexported: bool,
) -> Option<Diagnostic> {
    if is_private {
        if check_for_unexported {
            let maybe_fix = maybe_function_def.map(|function_def| {
                let mut builder = SourceChangeBuilder::new(function_def.file.file_id);
                helpers::ExportBuilder::new(
                    sema,
                    function_def.file.file_id,
                    ExportForm::Functions,
                    &[function_def.name],
                    &mut builder,
                )
                .finish();

                fix(
                    "export_function",
                    format!("Export the function `{function_name}`").as_str(),
                    builder.finish(),
                    range,
                )
            });
            let mut diagnostic = Diagnostic::new(
                DiagnosticCode::UnexportedFunction,
                format!("Function '{}' is not exported.", function_name),
                range,
            )
            .with_severity(Severity::Warning)
            .with_ignore_fix(sema, file_id);

            maybe_fix.inspect(|fix| {
                diagnostic.add_fix(fix.clone());
            });

            Some(diagnostic)
        } else {
            None
        }
    } else {
        Some(
            Diagnostic::new(
                DiagnosticCode::UndefinedFunction,
                format!("Function '{}' is undefined.", function_name),
                range,
            )
            .with_severity(Severity::Warning)
            .with_ignore_fix(sema, file_id),
        )
    }
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::DiagnosticsConfig;
    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;
    use crate::tests::check_nth_fix;

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
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'nocallback:behaviour_info/1' is undefined.
    nonexisting:behaviour_info(callbacks),
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'nonexisting:behaviour_info/1' is undefined.

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
    fn test_private() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    dependency:exists(),
    dependency:private().
%%  ^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'dependency:private/0' is not exported.
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -export([exists/0]).
  exists() -> ok.
  private() -> ok.
            "#,
        )
    }

    #[test]
    fn test_private_same_module() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    ?MODULE:private(),
%%  ^^^^^^^^^^^^^^^ 💡 warning: Function 'main:private/0' is not exported.
    main:private().
%%  ^^^^^^^^^^^^ 💡 warning: Function 'main:private/0' is not exported.

  private() -> ok.
            "#,
        )
    }

    #[test]
    fn remote_call_to_header() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> main:bar().
%%       ^^^^^^^^ 💡 warning: Function 'main:bar/0' is not exported.

//- /src/header.hrl
  bar() -> ok.
"#,
        );
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

    #[test]
    fn test_export_fix_ignore() {
        check_fix(
            r#"
//- /src/main.erl
-module(main).

main() ->
  dep:exists(),
  dep:pr~ivate().

//- /src/dep.erl
-module(dep).
-export([exists/0]).
exists() -> ok.
private() -> ok.
"#,
            expect![[r#"
-module(main).

main() ->
  dep:exists(),
  % elp:ignore W0026 (unexported_function)
  dep:private().

"#]],
        )
    }

    #[test]
    fn test_export_fix() {
        check_nth_fix(
            1,
            r#"
//- /src/main.erl
-module(main).

main() ->
  dep:exists(),
  dep:pr~ivate().

exists() -> ok.
//- /src/dep.erl
-module(dep).
-export([exists/0]).
exists() -> ok.
private() -> ok.
"#,
            expect![[r#"
-module(dep).
-export([exists/0, private/0]).
exists() -> ok.
private() -> ok.
"#]],
            DiagnosticsConfig::default().set_experimental(true),
            &vec![],
            crate::tests::IncludeCodeActionAssists::Yes,
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
%%      ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'dependency:not_exists/1' is undefined.
    fun dependency:module_info/2}.
%%      ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'dependency:module_info/2' is undefined.
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }
}
