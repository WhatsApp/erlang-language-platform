/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unavailable-function
//
// Return a warning when calling a function which exists, but lives in an
// application that is not reachable from the calling application's declared
// dependencies. This is the function-level counterpart of `unavailable_type`
// (W0059).
//
// Whereas `undefined_function` (W0017) asks "does `M:F/A` exist *anywhere* in
// the project?", this diagnostic asks "does `M:F/A` exist in a *declared
// dependency* of the caller?".
//
// Currently only available for Buck projects

use std::borrow::Cow;

use elp_ide_db::elp_base_db::AppData;
use elp_project_model::AppName;
use hir::Semantic;

use super::DiagnosticCode;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::lazy_function_matches;

pub(crate) struct UnavailableFunctionLinter;

impl Linter for UnavailableFunctionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnavailableFunction
    }
    fn description(&self) -> &'static str {
        "Function is not available through dependencies."
    }
    fn is_enabled(&self) -> bool {
        false
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    function_label: String,
    defining_app: String,
    referencing_app: String,
    referencing_target: String,
}

impl FunctionCallLinter for UnavailableFunctionLinter {
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
            "The function '{}' is defined in application '{}', but the application is not a dependency of '{}' (defined in '{}').",
            context.function_label,
            context.defining_app,
            context.referencing_app,
            context.referencing_target
        ))
    }

    fn check_match(&self, context: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        if let hir::CallTarget::Local { .. } = context.target {
            return None;
        }

        let sema = context.in_clause.sema;
        let def_fb = context.in_clause;
        let file_id = def_fb.file_id();
        let arity = context.args.arity();

        let referencing_app_data = sema.db.file_app_data(file_id)?;
        let referencing_target = referencing_app_data.buck_target_name.as_ref()?;

        // A call we cannot resolve is either undefined (W0017's job) or
        // dynamic. Either way there is no target app to check.
        let function_def = context
            .target
            .resolve_call(arity, sema, file_id, &def_fb.body())?;
        let defining_app_data = sema.db.file_app_data(function_def.file.file_id)?;

        let referencing_app = &referencing_app_data.name;
        let defining_app = &defining_app_data.name;

        if is_app_available(
            sema,
            &referencing_app_data,
            referencing_target,
            referencing_app,
            defining_app,
        ) {
            return None;
        }

        Some(Context {
            function_label: context.target.label(arity, &def_fb.body())?.to_string(),
            defining_app: defining_app.to_string(),
            referencing_app: referencing_app.to_string(),
            referencing_target: referencing_target.to_string(),
        })
    }
}

pub static LINTER: UnavailableFunctionLinter = UnavailableFunctionLinter;

fn is_app_available(
    sema: &Semantic,
    referencing_app_data: &AppData,
    referencing_target: &String,
    referencing_app: &AppName,
    defining_app: &AppName,
) -> bool {
    if referencing_app == defining_app {
        return true;
    }

    match &sema
        .db
        .project_data(referencing_app_data.project_id)
        .project_data(sema.db)
        .include_mapping
    {
        // `is_dep` treats OTP applications as always available, and walks the
        // dependency graph transitively. Transitive reachability is weaker than
        // checking direct dependencies only, but it is what `unavailable_type`
        // already uses, so both boundary diagnostics agree on what "reachable"
        // means. Tightening it is a possible follow-up.
        Some(include_mapping) => include_mapping.is_dep(referencing_target, defining_app),
        // No dependency graph (non-buck project) - be conservative and allow it.
        None => true,
    }
}

#[cfg(test)]
mod tests {

    use crate::DiagnosticsConfig;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests::check_diagnostics_with_config;

    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::UnavailableFunction)
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::NoSize);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn call_into_declared_dep_is_ok() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  main() ->
    app_b:exists().
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn call_into_undeclared_dep_is_reported() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  main() ->
    app_b:exists(),
    app_c:exists().
%%  ^^^^^^^^^^^^ warning: W0085: The function 'app_c:exists/0' is defined in application 'app_c', but the application is not a dependency of 'app_a' (defined in 'cell//app_a:lib').
%%             | 💡 <suppression>
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -compile(export_all).
  exists() -> ok.
//- /app_c/src/app_c.erl app:app_c buck_target:cell//app_c:lib
  -module(app_c).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn call_within_same_app_is_ok() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib
  -module(main).
  main() ->
    other:exists().
//- /app_a/src/other.erl app:app_a buck_target:cell//app_a:lib
  -module(other).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn transitive_dep_is_ok() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  main() ->
    app_c:exists().
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib deps:app_c
  -module(app_b).
  -compile(export_all).
  exists() -> ok.
//- /app_c/src/app_c.erl app:app_c buck_target:cell//app_c:lib
  -module(app_c).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn undefined_function_is_not_reported() {
        // An unresolvable call is W0017's business, not ours.
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  main() ->
    app_c:not_exists().
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn dynamic_module_is_not_reported() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  main(Callback) ->
    Callback:exists().
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn non_buck_project_is_not_reported() {
        // Without buck metadata there is no dependency graph to check against.
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    dependency:exists().
//- /src/dependency.erl
  -module(dependency).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }

    #[test]
    fn can_be_suppressed() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  main() ->
    % elp:ignore W0085
    app_c:exists().
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -compile(export_all).
  exists() -> ok.
//- /app_c/src/app_c.erl app:app_c buck_target:cell//app_c:lib
  -module(app_c).
  -compile(export_all).
  exists() -> ok.
            "#,
        )
    }
}
