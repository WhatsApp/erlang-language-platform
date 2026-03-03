/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileId;
use hir::Expr;
use hir::Literal;
use hir::Semantic;

use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::FunctionMatch;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::lazy_function_matches;

/// List of modules that should not be mocked in tests.
#[cfg(not(test))]
const RESTRICTED_MODULES: &[&str] = &[]; // @oss-only
// @fb-only: const RESTRICTED_MODULES: &[&str] = crate::diagnostics::meta_only::MECK_RESTRICTED_MODULES;

#[cfg(test)]
const RESTRICTED_MODULES: &[&str] = &["module1", "module2"];

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub(crate) struct MeckRestrictedContext {
    pub module_names: Vec<String>,
}

pub(crate) struct MeckRestrictedLinter;

impl Linter for MeckRestrictedLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MeckRestricted
    }

    fn description(&self) -> &'static str {
        "Mocking this module is almost always the wrong approach"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Error
    }
}

impl FunctionCallLinter for MeckRestrictedLinter {
    type Context = MeckRestrictedContext;

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        let modules = context.module_names.join("', '");
        Cow::Owned(format!(
            "Mocking '{}' is almost always the wrong approach",
            modules
        ))
    }

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![
            FunctionMatch::mf("meck", "expect"),
            FunctionMatch::mf("meck", "new"),
        ]]
    }

    fn check_match(&self, ctx: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        let first_arg = ctx.args.get(0)?;

        // Check if first argument is a restricted module atom directly
        if let Some(name) = get_restricted_module_name(ctx.sema, &first_arg, ctx.in_clause) {
            return Some(MeckRestrictedContext {
                module_names: vec![name],
            });
        }

        // Check if first argument is a list containing restricted module atoms
        let names = get_restricted_modules_from_list(ctx.sema, &first_arg, ctx.in_clause);
        if !names.is_empty() {
            return Some(MeckRestrictedContext {
                module_names: names,
            });
        }

        None
    }
}

fn get_restricted_module_name(
    sema: &Semantic,
    expr_id: &hir::ExprId,
    in_clause: &hir::InFunctionClauseBody<&hir::FunctionDef>,
) -> Option<String> {
    if let Expr::Literal(Literal::Atom(atom)) = &in_clause[*expr_id] {
        let atom_str = atom.as_string(sema.db.upcast());
        if RESTRICTED_MODULES.contains(&atom_str.as_str()) {
            return Some(atom_str);
        }
    }
    None
}

fn get_restricted_modules_from_list(
    sema: &Semantic,
    expr_id: &hir::ExprId,
    in_clause: &hir::InFunctionClauseBody<&hir::FunctionDef>,
) -> Vec<String> {
    let mut names = Vec::new();
    if let Expr::List { exprs, .. } = &in_clause[*expr_id] {
        for element in exprs {
            if let Some(name) = get_restricted_module_name(sema, element, in_clause) {
                names.push(name);
            }
        }
    }
    names
}

pub static LINTER: MeckRestrictedLinter = MeckRestrictedLinter;

#[cfg(test)]
mod tests {
    use crate::DiagnosticsConfig;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn test_meck_expect() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction),
            r#"
            -module(main).
            -export([test/0]).

            test() ->
                meck:expect(module1, function1, 1, ok),
            %%  ^^^^^^^^^^^ 💡 error: W0068: Mocking 'module1' is almost always the wrong approach
                meck:expect(module1, function2, fun(_) -> ok end),
            %%  ^^^^^^^^^^^ 💡 error: W0068: Mocking 'module1' is almost always the wrong approach

                meck:expect(module2, function1, 3, true),
            %%  ^^^^^^^^^^^ 💡 error: W0068: Mocking 'module2' is almost always the wrong approach
                meck:expect(module2, function2, fun(_, _, _) -> false end),
            %%  ^^^^^^^^^^^ 💡 error: W0068: Mocking 'module2' is almost always the wrong approach

                ok.
            "#,
        );
    }

    #[test]
    fn test_meck_new_single() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction),
            r#"
            -module(main).
            -export([test/0]).

            test() ->
                meck:new(module1, [passthrough]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module1' is almost always the wrong approach
                meck:new(module1, [passthrough, no_link]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module1' is almost always the wrong approach

                meck:new(module2, [passthrough]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module2' is almost always the wrong approach
                meck:new(module2, [passthrough, no_link]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module2' is almost always the wrong approach

                ok.
            "#,
        );
    }

    #[test]
    fn test_meck_new_in_list() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction),
            r#"
            -module(main).
            -export([test/0]).

            test() ->
                meck:new([module1], [passthrough]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module1' is almost always the wrong approach
                meck:new([other, module1], [passthrough]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module1' is almost always the wrong approach
                meck:new([module2, module1, another], [passthrough]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module2', 'module1' is almost always the wrong approach
                meck:new([module2, another], [passthrough]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module2' is almost always the wrong approach
                meck:new([foo, module2, bar], [passthrough]),
            %%  ^^^^^^^^ 💡 error: W0068: Mocking 'module2' is almost always the wrong approach

                ok.
            "#,
        );
    }

    #[test]
    fn test_meck_other_modules_no_warning() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction),
            r#"
            -module(main).
            -export([test/0]).

            test() ->
                meck:new(other_module, [passthrough]),
                meck:new([foo, bar], [passthrough]),

                meck:expect(some_module, function1, 1, ok),
                meck:expect(another, function2, fun(_) -> ok end),

                ok.
            "#,
        );
    }
}
