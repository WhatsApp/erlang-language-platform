/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: redundant_fun_wrapper
//
// Return a warning if an anonymous function is a redundant wrapper around
// a function call. Such closures can be replaced with direct fun references:
//   - fun(X) -> foo(X) end      → fun foo/1
//   - fun(X) -> mod:foo(X) end  → fun mod:foo/1
//   - fun(X) -> F(X) end        → F (where F is a fun variable)

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::Body;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FunctionClauseDef;
use hir::InFunctionClauseBody;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) struct RedundantFunWrapperLinter;

impl Linter for RedundantFunWrapperLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::RedundantFunWrapper
    }

    fn description(&self) -> &'static str {
        "Redundant anonymous function wrapper can be replaced with a fun reference."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Information
    }
}

/// The kind of redundant fun wrapper detected.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) enum RedundantFunKind {
    #[default]
    Unknown,
    /// fun(X) -> foo(X) end → fun foo/1
    LocalCall { fun_name: String, arity: u32 },
    /// fun(X) -> mod:foo(X) end → fun mod:foo/1
    RemoteCall {
        module: String,
        fun_name: String,
        arity: u32,
    },
    /// fun(X) -> F(X) end → F (where F is already a fun variable)
    FunVar { var_text: String },
}

impl RedundantFunKind {
    fn replacement(&self) -> Option<String> {
        match self {
            RedundantFunKind::Unknown => None,
            RedundantFunKind::LocalCall { fun_name, arity } => {
                Some(format!("fun {fun_name}/{arity}"))
            }
            RedundantFunKind::RemoteCall {
                module,
                fun_name,
                arity,
            } => Some(format!("fun {module}:{fun_name}/{arity}")),
            RedundantFunKind::FunVar { var_text } => Some(var_text.clone()),
        }
    }
}

impl GenericLinter for RedundantFunWrapperLinter {
    type Context = RedundantFunKind;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let mut res = Vec::new();

        sema.def_map(file_id)
            .get_function_clauses()
            .filter(|(_, def)| def.file.file_id == file_id)
            .for_each(|(_, def)| {
                let in_clause = def.in_clause(sema, def);
                in_clause.fold_clause(
                    Strategy {
                        macros: MacroStrategy::Expand,
                        parens: ParenStrategy::InvisibleParens,
                    },
                    (),
                    &mut |acc, ctx| {
                        if let AnyExpr::Expr(Expr::Closure { clauses, name }) = ctx.item
                            && name.is_none() // Skip named funs like `fun Name(X) -> ...`
                            && ctx.in_macro.is_none() // Skip closures inside macros
                            && let hir::AnyExprId::Expr(expr_id) = ctx.item_id
                            && let Some(range) = in_clause.range_for_expr(expr_id)
                            && range.file_id == file_id
                            && let Some(kind) = check_redundant_wrapper(
                                sema,
                                &in_clause,
                                file_id,
                                &in_clause.body(),
                                &clauses,
                            )
                        {
                            res.push(GenericLinterMatchContext {
                                range: range.range,
                                context: kind,
                            });
                        }
                        acc
                    },
                );
            });

        if res.is_empty() { None } else { Some(res) }
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        match context {
            RedundantFunKind::Unknown => Cow::Borrowed(self.description()),
            RedundantFunKind::LocalCall { fun_name, arity } => Cow::Owned(format!(
                "A reference to `fun {fun_name}/{arity}` can be taken directly."
            )),
            RedundantFunKind::RemoteCall {
                module,
                fun_name,
                arity,
            } => Cow::Owned(format!(
                "A reference to `fun {module}:{fun_name}/{arity}` can be taken directly."
            )),
            RedundantFunKind::FunVar { var_text } => Cow::Owned(format!(
                "Redundant wrapper around `{var_text}` can be removed."
            )),
        }
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let replacement = context.replacement()?;
        let mut builder = SourceChangeBuilder::new(file_id);
        builder.replace(range, replacement.clone());

        let fix_label = match context {
            RedundantFunKind::LocalCall { fun_name, arity } => {
                format!("Replace with `fun {fun_name}/{arity}`")
            }
            RedundantFunKind::RemoteCall {
                module,
                fun_name,
                arity,
            } => {
                format!("Replace with `fun {module}:{fun_name}/{arity}`")
            }
            RedundantFunKind::FunVar { var_text } => {
                format!("Replace with `{var_text}`")
            }
            RedundantFunKind::Unknown => return None,
        };

        Some(vec![fix(
            "redundant_fun_wrapper",
            &fix_label,
            builder.finish(),
            range,
        )])
    }
}

/// Check if a closure is a redundant wrapper.
/// Returns the kind of redundant wrapper if valid, None otherwise.
fn check_redundant_wrapper(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionClauseDef>,
    file_id: FileId,
    body: &Body,
    clauses: &[hir::Clause],
) -> Option<RedundantFunKind> {
    // Must have exactly one clause
    if clauses.len() != 1 {
        return None;
    }

    let clause = &clauses[0];

    // Must have no guards
    if !clause.guards.is_empty() {
        return None;
    }

    // Body must have exactly one expression that is a Call
    if clause.exprs.len() != 1 {
        return None;
    }

    let body_expr_id = clause.exprs[0];
    let body_expr = &body[body_expr_id];

    let Expr::Call { target, args } = body_expr else {
        return None;
    };

    // Check that all parameters are simple Vars and match call args in order
    if !is_passthrough_call(body, &clause.pats, args) {
        return None;
    }

    let arity = clause.pats.len() as u32;

    // Helper to get source text from a file range
    let get_source_text = |range: &FileRange| -> Option<String> {
        if range.file_id != file_id {
            // Comes from macro expansion, skip
            return None;
        }
        let source_file = sema.parse(file_id);
        Some(
            source_file
                .value
                .syntax()
                .text()
                .slice(range.range)
                .to_string(),
        )
    };

    // Determine the kind based on the call target
    match target {
        CallTarget::Local { name } => {
            // Could be a local function call or a variable call
            let name_expr = &body[*name];
            match name_expr {
                Expr::Literal(hir::Literal::Atom(_)) => {
                    // Get the source text for the function name
                    let name_range = in_clause.range_for_any(AnyExprId::Expr(*name))?;
                    let fun_name = get_source_text(&name_range)?;
                    Some(RedundantFunKind::LocalCall { fun_name, arity })
                }
                Expr::Var(_) => {
                    // This is a call via a variable like F(X)
                    let name_range = in_clause.range_for_any(AnyExprId::Expr(*name))?;
                    let var_text = get_source_text(&name_range)?;
                    Some(RedundantFunKind::FunVar { var_text })
                }
                _ => None,
            }
        }
        CallTarget::Remote { module, name, .. } => {
            let module_expr = &body[*module];
            let name_expr = &body[*name];

            // Only handle static module:function calls (both must be atoms)
            if !matches!(module_expr, Expr::Literal(hir::Literal::Atom(_))) {
                return None;
            }
            if !matches!(name_expr, Expr::Literal(hir::Literal::Atom(_))) {
                return None;
            }

            // Get source text for module and function name
            let module_range = in_clause.range_for_any(AnyExprId::Expr(*module))?;
            let name_range = in_clause.range_for_any(AnyExprId::Expr(*name))?;

            // Get source text - this will skip macro expansions
            let module_text = get_source_text(&module_range)?;
            let fun_name = get_source_text(&name_range)?;

            Some(RedundantFunKind::RemoteCall {
                module: module_text,
                fun_name,
                arity,
            })
        }
    }
}

/// Check if the call arguments match the closure parameters in order.
/// All parameters must be simple Vars, and each arg must be the same Var.
fn is_passthrough_call(body: &hir::Body, pats: &[hir::PatId], args: &[ExprId]) -> bool {
    // Must have same arity
    if pats.len() != args.len() {
        return false;
    }

    for (pat_id, arg_id) in pats.iter().zip(args.iter()) {
        let pat = &body[*pat_id];

        // Parameter must be a simple Var (not a pattern like {X, Y})
        let Pat::Var(pat_var) = pat else {
            return false;
        };

        let arg_expr = &body[*arg_id];

        // Argument must be a Var
        let Expr::Var(arg_var) = arg_expr else {
            return false;
        };

        // Must be the same variable
        if pat_var != arg_var {
            return false;
        }
    }

    true
}

pub static LINTER: RedundantFunWrapperLinter = RedundantFunWrapperLinter;

#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::RedundantFunWrapper
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    // -------------------------------------------------------------------------
    // Detection Tests (should flag)
    // -------------------------------------------------------------------------

    #[test]
    fn detects_local_call_arity_1() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            foo(X) -> X.
            use_fun(L) ->
                lists:map(fun(X) -> foo(X) end, L).
            %%            ^^^^^^^^^^^^^^^^^^^^ 💡 information: W0071: A reference to `fun foo/1` can be taken directly.
            "#,
        )
    }

    #[test]
    fn detects_local_call_arity_2() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            add(X, Y) -> X + Y.
            use_fun(L) ->
                lists:foldl(fun(X, Y) -> add(X, Y) end, 0, L).
            %%              ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0071: A reference to `fun add/2` can be taken directly.
            "#,
        )
    }

    #[test]
    fn detects_remote_call_arity_1() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            use_fun(L) ->
                lists:map(fun(X) -> erlang:abs(X) end, L).
            %%            ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0071: A reference to `fun erlang:abs/1` can be taken directly.
            "#,
        )
    }

    #[test]
    fn detects_remote_call_arity_2() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            use_fun(L) ->
                lists:foldl(fun(X, Y) -> erlang:max(X, Y) end, 0, L).
            %%              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0071: A reference to `fun erlang:max/2` can be taken directly.
            "#,
        )
    }

    #[test]
    fn detects_fun_var() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/2]).

            use_fun(F, L) ->
                lists:map(fun(X) -> F(X) end, L).
            %%            ^^^^^^^^^^^^^^^^^^ 💡 information: W0071: Redundant wrapper around `F` can be removed.
            "#,
        )
    }

    #[test]
    fn detects_fun_var_arity_2() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/2]).

            use_fun(F, L) ->
                lists:foldl(fun(Rec, Acc) -> F(Rec, Acc) end, [], L).
            %%              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0071: Redundant wrapper around `F` can be removed.
            "#,
        )
    }

    // -------------------------------------------------------------------------
    // Negative Tests (should NOT flag)
    // -------------------------------------------------------------------------

    #[test]
    fn ignores_extra_captured_arg() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/2]).

            foo(X, Y) -> X + Y.
            use_fun(Y, L) ->
                lists:map(fun(X) -> foo(X, Y) end, L).
            "#,
        )
    }

    #[test]
    fn ignores_swapped_args() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            foo(X, Y) -> X - Y.
            use_fun(L) ->
                lists:foldl(fun(X, Y) -> foo(Y, X) end, 0, L).
            "#,
        )
    }

    #[test]
    fn ignores_nested_call() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            foo(X) -> X.
            bar(X) -> X.
            use_fun(L) ->
                lists:map(fun(X) -> foo(bar(X)) end, L).
            "#,
        )
    }

    #[test]
    fn ignores_non_call_body() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            use_fun(L) ->
                lists:map(fun(X) -> X + 1 end, L).
            "#,
        )
    }

    #[test]
    fn ignores_pattern_matching_in_params() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            foo(X, Y) -> X + Y.
            use_fun(L) ->
                lists:map(fun({X, Y}) -> foo(X, Y) end, L).
            "#,
        )
    }

    #[test]
    fn ignores_guarded_closure() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            foo(X) -> X.
            use_fun(L) ->
                lists:map(fun(X) when X > 0 -> foo(X) end, L).
            "#,
        )
    }

    #[test]
    fn ignores_multi_clause_closure() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            use_fun(L) ->
                lists:map(fun(0) -> zero; (X) -> X end, L).
            "#,
        )
    }

    #[test]
    fn detects_underscore_prefixed_params() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            foo(_X) -> ok.
            use_fun(L) ->
                lists:map(fun(_X) -> foo(_X) end, L).
            %%            ^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0071: A reference to `fun foo/1` can be taken directly.
            "#,
        )
    }

    #[test]
    fn ignores_named_fun() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/0]).

            foo(X) -> X.
            use_fun() ->
                fun Loop(X) -> foo(X) end.
            "#,
        )
    }

    // -------------------------------------------------------------------------
    // Fix Tests
    // -------------------------------------------------------------------------

    #[test]
    fn fixes_local_call() {
        check_fix(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            foo(X) -> X.
            use_fun(L) ->
                lists:map(fun(~X) -> foo(X) end, L).
            "#,
            expect![[r#"
            -module(test).
            -export([use_fun/1]).

            foo(X) -> X.
            use_fun(L) ->
                lists:map(fun foo/1, L).
            "#]],
        )
    }

    #[test]
    fn fixes_remote_call() {
        check_fix(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            use_fun(L) ->
                lists:map(fun(X~) -> erlang:abs(X) end, L).
            "#,
            expect![[r#"
            -module(test).
            -export([use_fun/1]).

            use_fun(L) ->
                lists:map(fun erlang:abs/1, L).
            "#]],
        )
    }

    #[test]
    fn fixes_fun_var() {
        check_fix(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/2]).

            use_fun(F, L) ->
                lists:map(fun(X) -> F~(X) end, L).
            "#,
            expect![[r#"
            -module(test).
            -export([use_fun/2]).

            use_fun(F, L) ->
                lists:map(F, L).
            "#]],
        )
    }

    #[test]
    fn fixes_arity_2() {
        check_fix(
            r#"
            //- /src/test.erl
            -module(test).
            -export([use_fun/1]).

            add(X, Y) -> X + Y.
            use_fun(L) ->
                lists:foldl(fun(X~, Y) -> add(X, Y) end, 0, L).
            "#,
            expect![[r#"
            -module(test).
            -export([use_fun/1]).

            add(X, Y) -> X + Y.
            use_fun(L) ->
                lists:foldl(fun add/2, 0, L).
            "#]],
        )
    }
}
