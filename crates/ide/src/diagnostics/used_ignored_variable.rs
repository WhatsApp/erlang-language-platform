/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: used_ignored_variable
//!
//! Return a diagnostic when a variable whose name starts with `_`, which by
//! convention marks it as ignored, is used after being bound, as in
//! `f(_X) -> _X + 1`. The compiler does not warn about unused variables with
//! such names, so the convention hides real mistakes and misleads readers.

use std::borrow::Cow;
use std::ops::Range;

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::TextRange;
use fxhash::FxHashSet;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::Pat;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use hir::Var;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::resolver::Resolver;

use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct UsedIgnoredVariableLinter;

impl Linter for UsedIgnoredVariableLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UsedIgnoredVariable
    }

    fn description(&self) -> &'static str {
        "A variable marked as ignored is used."
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Context {
    name: String,
}

const STRATEGY: Strategy = Strategy {
    macros: MacroStrategy::ExpandButIncludeMacroCall,
    parens: ParenStrategy::InvisibleParens,
};

const ARG_STRATEGY: Strategy = Strategy {
    macros: MacroStrategy::DoNotExpand,
    parens: ParenStrategy::InvisibleParens,
};

impl GenericLinter for UsedIgnoredVariableLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut res = Vec::new();
        sema.for_each_function(file_id, |def| {
            let def_fb = def.in_function_body(sema, def);
            for (clause_id, _) in def_fb.clauses() {
                let in_clause = def_fb.in_clause(clause_id);
                check_clause(sema, file_id, in_clause, &mut res);
            }
        });
        res.sort_by_key(|m| m.range.range.start());
        Some(res)
    }

    fn match_description(&self, context: &Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Variable `{}` is used, but its leading `_` marks it as ignored.",
            context.name
        ))
    }
}

pub(crate) static LINTER: UsedIgnoredVariableLinter = UsedIgnoredVariableLinter;

fn check_clause(
    sema: &Semantic,
    file_id: FileId,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    res: &mut Vec<GenericLinterMatchContext<Context>>,
) {
    let function_range = in_clause.range();
    let text = sema.db.file(file_id).text(sema.db);
    // Occurrences written in the function, as opposed to inside a macro body.
    let mut occurrences: Vec<(Var, TextRange, bool)> = Vec::new();
    let mut record = |var: Var, range: Option<FileRange>, is_use: bool| {
        if var.is_underscore_prefixed()
            && !var.is_anonymous()
            && !var.is_double_underscore_prefixed()
            && let Some(range) = range
            && range.file_id == file_id
            && function_range.contains_range(range.range)
            && text.get(Range::<usize>::from(range.range)) == Some(var.as_string().as_str())
        {
            occurrences.push((var, range.range, is_use));
        }
    };
    let resolver = in_clause.resolver();
    let mut macro_args = Vec::new();
    let mut macro_calls = Vec::new();
    in_clause.fold_clause(STRATEGY, (), &mut |_acc, fold_ctx| match fold_ctx.item {
        AnyExpr::Expr(Expr::Var(var)) => {
            record(var, in_clause.range_for_any(fold_ctx.item_id), true)
        }
        // A pattern variable that is already bound matches its value, so it is
        // a use. Inside a macro expansion, scoping can differ from the
        // arguments as written, so patterns in macro arguments are checked
        // separately below.
        AnyExpr::Pat(Pat::Var(var)) => {
            let is_use = fold_ctx.in_macro.is_none()
                && matches!(fold_ctx.item_id, AnyExprId::Pat(pat_id)
                    if resolver.resolve_pat_id(&var, pat_id).is_some_and(|defs| !defs.contains(&pat_id)));
            record(var, in_clause.range_for_any(fold_ctx.item_id), is_use)
        }
        // The expansion maps a substituted argument back to the whole macro
        // call, so look at the arguments as written.
        AnyExpr::Expr(Expr::MacroCall { args, .. }) => {
            macro_calls.extend(in_clause.range_for_any(fold_ctx.item_id));
            macro_args.extend(args.iter().map(|&arg| (arg, None)))
        }
        AnyExpr::Pat(Pat::MacroCall { args, .. }) => {
            let call = in_clause.range_for_any(fold_ctx.item_id);
            macro_calls.extend(call);
            macro_args.extend(args.iter().map(|&arg| (arg, Some(call))))
        }
        _ => {}
    });
    let mut matched = FxHashSet::default();
    for (arg, pattern_call) in macro_args {
        in_clause.fold_expr(ARG_STRATEGY, arg, (), &mut |_acc, fold_ctx| {
            match (fold_ctx.item, fold_ctx.item_id) {
                (AnyExpr::Expr(Expr::Var(var)), AnyExprId::Expr(expr_id)) => {
                    let is_use = match pattern_call {
                        None => true,
                        Some(call) => bound_outside(in_clause, &resolver, &var, expr_id, call),
                    };
                    record(var, in_clause.range_for_any(fold_ctx.item_id), is_use);
                }
                // A pattern inside the argument, as in `?M(case Y of _X -> ok end)`.
                // Bindings made inside macro calls can be scoped differently in
                // the expansion, so only a binding outside every call counts.
                (AnyExpr::Pat(Pat::Var(var)), AnyExprId::Pat(pat_id))
                    if bound_outside_macros(in_clause, &resolver, &var, pat_id, &macro_calls) =>
                {
                    let range = in_clause.range_for_any(fold_ctx.item_id);
                    matched.extend(range.map(|range| range.range));
                    record(var, range, true);
                }
                _ => {}
            }
        });
    }
    // A macro argument the expansion uses as a pattern, as in `?assertMatch`,
    // binds the variable instead of using it, unless it is already bound.
    let bindings: FxHashSet<TextRange> = occurrences
        .iter()
        .filter(|(_, range, is_use)| !is_use && !matched.contains(range))
        .map(|(_, range, _)| *range)
        .collect();
    let mut uses: Vec<(Var, TextRange)> = occurrences
        .into_iter()
        .filter(|(_, range, is_use)| *is_use && !bindings.contains(range))
        .map(|(var, range, _)| (var, range))
        .collect();
    uses.sort_by_key(|(_, range)| range.start());
    let mut reported = FxHashSet::default();
    res.extend(
        uses.into_iter()
            .filter(|(var, _)| reported.insert(*var))
            .map(|(var, range)| GenericLinterMatchContext {
                range: FileRange { file_id, range },
                context: Context {
                    name: var.as_string(),
                },
            }),
    );
}

/// Whether `var`, an argument of a macro call in a pattern, is bound outside
/// that call, so the pattern matches its value instead of binding it.
fn bound_outside(
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    resolver: &Resolver,
    var: &Var,
    expr_id: ExprId,
    call: Option<FileRange>,
) -> bool {
    let Some(call) = call else {
        return false;
    };
    resolver.resolve_expr_id(var, expr_id).is_some_and(|defs| {
        defs.iter().any(|def| {
            in_clause
                .range_for_any(AnyExprId::Pat(*def))
                .is_some_and(|def_range| !call.range.contains_range(def_range.range))
        })
    })
}

/// Whether `var`, a pattern variable, is already bound by a pattern written
/// outside every macro call in the clause, so the pattern matches its value.
fn bound_outside_macros(
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    resolver: &Resolver,
    var: &Var,
    pat_id: PatId,
    macro_calls: &[FileRange],
) -> bool {
    resolver.resolve_pat_id(var, pat_id).is_some_and(|defs| {
        !defs.contains(&pat_id)
            && defs.iter().any(|def| {
                in_clause
                    .range_for_any(AnyExprId::Pat(*def))
                    .is_some_and(|def_range| {
                        !macro_calls
                            .iter()
                            .any(|call| call.range.contains_range(def_range.range))
                    })
            })
    })
}

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction)
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        check_diagnostics_with_config(config(), fixture)
    }

    #[test]
    fn used_ignored_variables() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/2]).
            f(_X, Y) when _X > 0 ->
            %%            ^^ warning: W0095: Variable `_X` is used, but its leading `_` marks it as ignored.
            %%             | 💡 <suppression>
                {_Z, _} = Y,
                [_X, _Z, _X];
            %%       ^^ warning: W0095: Variable `_Z` is used, but its leading `_` marks it as ignored.
            %%        | 💡 <suppression>
            f(_, _Y) ->
                _Y.
            %%  ^^ warning: W0095: Variable `_Y` is used, but its leading `_` marks it as ignored.
            %%   | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn ignores_unused_and_macro_variables() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1]).
            -define(SWAP(A), begin {_A, _B} = A, {_B, _A} end).
            -define(ID(A), A).
            f(_X) ->
                {_Y, _} = ?SWAP({1, 2}),
                __Injected = ?ID(ok),
                __Injected.
            "#,
        );
    }

    #[test]
    fn macro_argument() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1]).
            -define(ID(A), A).
            f(_X) -> ?ID(_X).
            %%           ^^ warning: W0095: Variable `_X` is used, but its leading `_` marks it as ignored.
            %%            | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn pattern_macro_argument() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1, g/1]).
            -define(ID(A), A).
            f(?ID(_X)) -> _X.
            %%            ^^ warning: W0095: Variable `_X` is used, but its leading `_` marks it as ignored.
            %%             | 💡 <suppression>
            g(?ID(_Y)) -> ok.
            "#,
        );
    }

    #[test]
    fn bound_variable_in_pattern() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/2, g/2, h/1]).
            -define(ID(A), A).
            f(_X, Y) ->
                case Y of
                    _X -> ok;
            %%      ^^ warning: W0095: Variable `_X` is used, but its leading `_` marks it as ignored.
            %%       | 💡 <suppression>
                    _ -> error
                end.
            g(_X, Y) ->
                case Y of
                    ?ID(_X) -> ok;
            %%          ^^ warning: W0095: Variable `_X` is used, but its leading `_` marks it as ignored.
            %%           | 💡 <suppression>
                    _ -> error
                end.
            h(Y) ->
                case Y of
                    {_A} -> ok;
                    [_A] -> ok
                end.
            "#,
        );
    }

    #[test]
    fn bound_variable_in_pattern_in_macro_argument() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/2, g/1, h/1]).
            -define(ID(A), A).
            -define(WRAP(E), (fun() -> E end)()).
            f(_X, Y) ->
                ?ID(case Y of
                        {_X} -> ok;
            %%           ^^ warning: W0095: Variable `_X` is used, but its leading `_` marks it as ignored.
            %%            | 💡 <suppression>
                        _ -> error
                    end).
            g(Y) ->
                ?ID(case Y of
                        {_A} -> ok;
                        [_A] -> ok
                    end).
            h(Y) ->
                ?WRAP(case Y of {_B} -> ok end),
                ?WRAP(case Y of {_B} -> ok end).
            "#,
        );
    }

    #[test]
    fn rebinding_in_separate_funs() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/0, g/0]).
            -define(WRAP(E), (fun() -> E end)()).
            f() -> (fun() -> _A = 1 end)(), (fun() -> _A = 2 end)().
            g() -> ?WRAP(_B = 1), ?WRAP(_B = 2).
            "#,
        );
    }

    #[test]
    fn pattern_in_macro_argument() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1]).
            -define(MATCH(P, E), case E of P -> ok end).
            f(X) -> ?MATCH({_A, _}, X).
            "#,
        );
    }
}
