/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: undefined-module
//
// Return a warning when a call names a module which has no definition, but
// not the function it calls: `M:F(...)` or `fun M:F/A` with `F` not an atom,
// or a dynamic call such as `apply(M, F, Args)`. Calls naming both the module
// and the function are checked by `undefined_function` (W0017).

use std::borrow::Cow;

use elp_ide_db::elp_base_db::AritySource;
use elp_ide_db::elp_base_db::FileRange;
use hir::CallTarget;
use hir::ExprId;
use hir::Name;
use hir::sema::dynamic_calls::lookup_dynamic_pattern;

use super::DiagnosticCode;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::UseRange;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::undefined_function::is_non_strict_mock;
use crate::lazy_function_matches;

pub(crate) struct UndefinedModuleLinter;

impl Linter for UndefinedModuleLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UndefinedModule
    }
    fn description(&self) -> &'static str {
        "Module is undefined."
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    module: String,
    module_range: Option<FileRange>,
}

impl FunctionCallLinter for UndefinedModuleLinter {
    type Context = Context;

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::any()]]
    }

    fn excludes_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![
            FunctionMatch::m("lager"), // Lager functions are produced by parse transforms
        ]]
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("Module '{}' is undefined.", context.module))
    }

    fn match_range(&self, match_ctx: &MatchCtx<Self::Context>) -> FileRange {
        match_ctx
            .extra
            .module_range
            .unwrap_or_else(|| match_ctx.range(&UseRange::NameOnly))
    }

    fn check_match(&self, context: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        let sema = context.in_clause.sema;
        let file_id = context.in_clause.file_id();
        let module_expr = unresolved_call_module(context)?;
        let module = &context.in_clause[module_expr];
        let name = module.as_atom()?.as_name();
        if sema.resolve_module_expr(file_id, module).is_some()
            || is_non_strict_mock(sema, file_id, &name)
        {
            return None;
        }
        Some(Context {
            module: name.as_str().to_string(),
            module_range: context.in_clause.range_for_expr(module_expr),
        })
    }
}

/// The module expression of a call that names its module but not the function
/// it calls: `M:F(...)` or `fun M:F/A` with `F` not an atom, or a dynamic call
/// that calls `Module:Function` with `Args`, such as
/// `apply(Module, Function, Args)`.
pub(crate) fn unresolved_call_module(context: &CheckCallCtx<'_, ()>) -> Option<ExprId> {
    let def_fb = context.in_clause;
    match context.target {
        CallTarget::Remote { module, name, .. } if def_fb[*name].as_atom().is_none() => {
            Some(*module)
        }
        CallTarget::Remote { module, name, .. } => {
            dynamic_call_module(context, Some(&def_fb.as_atom_name(module)?), name)
        }
        CallTarget::Local { name } => {
            let module = dynamic_call_module(context, None, name)?;
            let arity = context.args.arity();
            let defined_locally = context
                .target
                .resolve_call(arity, def_fb.sema, def_fb.file_id(), &def_fb.body())
                .is_some_and(|def| def.file.file_id == def_fb.file_id());
            (!defined_locally).then_some(module)
        }
    }
}

/// The `Module` argument of a call matching a dynamic call pattern that calls
/// `Module:Function` with `Args`, where `Module` is a single module.
fn dynamic_call_module(
    context: &CheckCallCtx<'_, ()>,
    module: Option<&Name>,
    function: &ExprId,
) -> Option<ExprId> {
    let def_fb = context.in_clause;
    let function = def_fb.as_atom_name(function)?;
    let pattern = lookup_dynamic_pattern(
        def_fb.sema.db.upcast(),
        module.map(Name::as_str),
        function.as_str(),
        context.args.arity() as usize,
    )?;
    if pattern.function_arg_index.is_none()
        || pattern.arity_source != AritySource::ListLength
        || !pattern.module_arg_shape.allows_atom()
    {
        return None;
    }
    context.args.get(pattern.module_arg_index? as usize)
}

pub static LINTER: UndefinedModuleLinter = UndefinedModuleLinter;

#[cfg(test)]
mod tests {

    use crate::DiagnosticsConfig;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::LintConfig;
    use crate::diagnostics::LinterConfig;
    use crate::tests::check_diagnostics_with_config;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::CrossNodeEval)
            .disable(DiagnosticCode::NoSize)
    }

    fn check_diagnostics(fixture: &str) {
        check_diagnostics_with_config(config(), fixture)
    }

    #[test]
    fn remote_call_with_variable_function() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main(F) ->
    dependency:F(),
    undefined_mod:F(),
%%  ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%              | 💡 <suppression>
    {fun dependency:F/0, fun undefined_mod:F/0}.
%%                           ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%                                       | 💡 <suppression>
//- /src/dependency.erl
  -module(dependency).
            "#,
        )
    }

    #[test]
    fn dynamic_calls() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main(Node, F, A) ->
    apply(dependency, F, A),
    apply(undefined_mod, F, A),
%%        ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%                    | 💡 <suppression>
    erlang:apply(undefined_mod, f, A),
%%               ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%                           | 💡 <suppression>
    spawn(undefined_mod, f, []),
%%        ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%                    | 💡 <suppression>
    spawn_opt(undefined_mod, f, [], []),
%%            ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%                        | 💡 <suppression>
    rpc:call(Node, undefined_mod, F, A).
%%                 ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%                             | 💡 <suppression>
//- /src/dependency.erl
  -module(dependency).
            "#,
        )
    }

    #[test]
    fn configured_dynamic_calls() {
        check_diagnostics(
            r#"
//- dynamic_call: my_rpc:call(_, Module, Function, Args)
//- /src/main.erl
  -module(main).
  main(Node, F, A) ->
    my_rpc:call(Node, undefined_mod, F, A).
%%                    ^^^^^^^^^^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%                                | 💡 <suppression>
//- /src/my_rpc.erl
  -module(my_rpc).
  -export([call/4]).
  call(_, _, _, _) -> ok.
            "#,
        )
    }

    #[test]
    fn calls_naming_the_function_are_not_reported() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    undefined_mod:f(),
    fun undefined_mod:f/0.
            "#,
        )
    }

    #[test]
    fn variable_modules_are_not_reported() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main(M, F, A) ->
    M:F(),
    apply(M, F, A).
            "#,
        )
    }

    #[test]
    fn calls_without_args_are_not_reported() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main(F) ->
    erlang:function_exported(undefined_mod, F, 0),
    code:ensure_loaded(undefined_mod).
            "#,
        )
    }

    #[test]
    fn local_apply_is_not_reported() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  -compile({no_auto_import, [apply/3]}).
  main(F, A) ->
    apply(undefined_mod, F, A).
  apply(M, F, A) -> {M, F, A}.
            "#,
        )
    }

    #[test]
    fn module_from_macro() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  -define(MOD, undefined_mod).
  main(F) ->
    ?MOD:F().
%%  ^^^^ warning: W0089: Module 'undefined_mod' is undefined.
%%     | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn non_strict_mocks_are_not_reported() {
        let mut lint_config = LintConfig::default();
        lint_config.linters.insert(
            DiagnosticCode::UndefinedModule,
            LinterConfig {
                include_tests: Some(true),
                ..Default::default()
            },
        );
        let config = config()
            .configure_diagnostics(&lint_config, &[], &[])
            .unwrap();
        check_diagnostics_with_config(
            config,
            r#"
//- /my_app/test/main_SUITE.erl extra:test
  -module(main_SUITE).
  init_per_suite(Config) ->
    meck:new(mocked, [non_strict]),
    Config.
  main(F) ->
    mocked:F(),
    strict_mock:F().
%%  ^^^^^^^^^^^ warning: W0089: Module 'strict_mock' is undefined.
%%            | 💡 <suppression>
//- /my_app/src/meck.erl
  -module(meck).
  -export([new/2]).
  new(_, _) -> ok.
            "#,
        )
    }
}
