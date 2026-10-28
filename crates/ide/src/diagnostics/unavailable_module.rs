/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unavailable-module
//
// Return a warning when a call names a module which exists, but lives in an
// application that is not reachable from the calling application's declared
// dependencies, and the call does not name the function it calls. This is the
// module-level counterpart of `unavailable_function` (W0085), for the calls
// that `undefined_module` (W0089) checks.
//
// Currently only available for Buck projects

use std::borrow::Cow;

use elp_ide_db::elp_base_db::DepKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::is_any_owner_reachable;

use super::DiagnosticCode;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::UseRange;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::undefined_module::unresolved_call_module;
use crate::lazy_function_matches;

pub(crate) struct UnavailableModuleLinter;

impl Linter for UnavailableModuleLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnavailableModule
    }
    fn description(&self) -> &'static str {
        "Module is not available through dependencies."
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    module: String,
    defining_app: String,
    referencing_app: String,
    referencing_target: String,
    module_range: Option<FileRange>,
}

impl FunctionCallLinter for UnavailableModuleLinter {
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
        Cow::Owned(format!(
            "The module '{}' is defined in application '{}', but the application is not a dependency of '{}' (defined in '{}').",
            context.module,
            context.defining_app,
            context.referencing_app,
            context.referencing_target
        ))
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
        let module_expr_id = unresolved_call_module(context)?;
        let module_expr = &context.in_clause[module_expr_id];

        let referencing_app_data = sema.db.file_app_data(file_id)?;
        let referencing_target = referencing_app_data.buck_target_name.as_ref()?;

        let module_file_id = sema.resolve_module_expr(file_id, module_expr)?.file.file_id;
        let defining_app_data = sema.db.file_app_data(module_file_id)?;

        // A call is a runtime edge, so the module's beam must be loadable in
        // this VM: an application only reached over the network will not do.
        if is_any_owner_reachable(
            sema.db.upcast(),
            &referencing_app_data,
            module_file_id,
            DepKind::Runtime,
        ) {
            return None;
        }

        Some(Context {
            module: module_expr.as_atom()?.as_name().as_str().to_string(),
            defining_app: defining_app_data.name.to_string(),
            referencing_app: referencing_app_data.name.to_string(),
            referencing_target: referencing_target.to_string(),
            module_range: context.in_clause.range_for_expr(module_expr_id),
        })
    }
}

pub static LINTER: UnavailableModuleLinter = UnavailableModuleLinter;

#[cfg(test)]
mod tests {

    use crate::DiagnosticsConfig;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests::check_diagnostics_with_config;

    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::NoSize);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn call_into_undeclared_dep_is_reported() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  main(F, A) ->
    main:F(),
    app_b:F(),
    app_c:F(),
%%  ^^^^^ warning: W0090: The module 'app_c' is defined in application 'app_c', but the application is not a dependency of 'app_a' (defined in 'cell//app_a:lib').
%%      | 💡 <suppression>
    apply(app_b, F, A),
    apply(app_c, F, A).
%%        ^^^^^ warning: W0090: The module 'app_c' is defined in application 'app_c', but the application is not a dependency of 'app_a' (defined in 'cell//app_a:lib').
%%            | 💡 <suppression>
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
//- /app_c/src/app_c.erl app:app_c buck_target:cell//app_c:lib
  -module(app_c).
            "#,
        )
    }

    #[test]
    fn calls_naming_the_function_are_not_reported() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib
  -module(main).
  main() ->
    app_c:exists().
//- /app_c/src/app_c.erl app:app_c buck_target:cell//app_c:lib
  -module(app_c).
  -export([exists/0]).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn call_into_distributed_dep_is_reported() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib distributed_deps:app_b
  -module(main).
  main(F) ->
    app_b:F().
%%  ^^^^^ warning: W0090: The module 'app_b' is defined in application 'app_b', but the application is not a dependency of 'app_a' (defined in 'cell//app_a:lib').
%%      | 💡 <suppression>
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
            "#,
        )
    }

    #[test]
    fn non_buck_projects_are_not_reported() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a
  -module(main).
  main(F) ->
    app_c:F().
//- /app_c/src/app_c.erl app:app_c
  -module(app_c).
            "#,
        )
    }
}
