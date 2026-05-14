/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint/fix: effect_free_statement
//!
//! Return a diagnostic if a statement is just a literal or a variable, and
//! offer to remove the statement as a fix.

use elp_ide_assists::Assist;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use super::Category;
use crate::codemod_helpers::next_statement_in_slot;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;

pub(crate) struct EffectFreeStatementLinter;

impl Linter for EffectFreeStatementLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::StatementHasNoEffect
    }

    fn description(&self) -> &'static str {
        "this statement has no effect"
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Context {
    statement_removal: Option<TextEdit>,
}

impl GenericLinter for EffectFreeStatementLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut res = Vec::new();
        sema.for_each_function(file_id, |def| {
            let def_fb = def.in_function_body(sema, def);
            def_fb.fold_function(
                // VisibleParens so a Paren-wrapped statement like `(blah),`
                // gets its own visit at the Paren ExprId. Under
                // InvisibleParens the fold short-circuits the Paren and
                // visits only the inner expression, whose source range
                // covers just `blah` rather than the parenthesised form
                // the user wrote.
                Strategy {
                    macros: MacroStrategy::ExpandButIncludeMacroCall,
                    parens: ParenStrategy::VisibleParens,
                },
                (),
                &mut |_acc, clause_id, ctx| {
                    let in_clause = def_fb.in_clause(clause_id);
                    if ctx.in_macro.is_none()
                        && !matches!(ctx.item, AnyExpr::Expr(Expr::MacroCall { .. }))
                        && let AnyExprId::Expr(expr_id) = ctx.item_id
                        && let Some(next_expr_id) =
                            next_statement_in_slot(in_clause, ctx.parent(), expr_id)
                        && has_no_effect(in_clause, &expr_id)
                        && let Some(curr_range) = in_clause.range_for_expr(expr_id)
                        && let Some(next_range) = in_clause.range_for_expr(next_expr_id)
                    {
                        let deletion_range =
                            TextRange::new(curr_range.range.start(), next_range.range.start());
                        res.push(GenericLinterMatchContext {
                            range: curr_range,
                            context: Context {
                                statement_removal: Some(TextEdit::delete(deletion_range)),
                            },
                        });
                    }
                },
            );
        });
        Some(res)
    }

    fn fixes(
        &self,
        context: &Context,
        range: TextRange,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let file_id = ctx.file_id;
        let statement_removal = context.statement_removal.as_ref()?;
        Some(vec![fix(
            "remove_statement",
            "Remove redundant statement",
            SourceChange::from_text_edit(file_id, statement_removal.clone()),
            range,
        )])
    }

    fn add_categories(&self, _context: &Context) -> Vec<Category> {
        vec![Category::SimplificationRule]
    }
}

pub(crate) static LINTER: EffectFreeStatementLinter = EffectFreeStatementLinter;

fn has_no_effect(def_fb: &InFunctionClauseBody<&FunctionDef>, expr_id: &ExprId) -> bool {
    let expr = &def_fb[*expr_id];
    match expr {
        Expr::Missing => false,

        Expr::Literal(_) => true,
        Expr::Binary { .. } => true,
        Expr::Var(_) => true,
        Expr::CaptureFun { .. } => true,
        Expr::Closure { .. } => true,

        Expr::Tuple { exprs } => exprs
            .iter()
            .all(|list_elem| has_no_effect(def_fb, list_elem)),

        Expr::List { exprs, .. } => exprs
            .iter()
            .all(|list_elem| has_no_effect(def_fb, list_elem)),

        Expr::Map { fields } => fields
            .iter()
            .all(|(k, v)| has_no_effect(def_fb, k) && has_no_effect(def_fb, v)),
        Expr::MapUpdate { .. } => {
            // Side-effect: may throw if not a map
            false
        }
        Expr::Comprehension { .. } => {
            // Side-effect: may throw due to generators types
            false
        }

        Expr::Record { fields, .. } => fields
            .iter()
            .all(|(_key, value)| has_no_effect(def_fb, value)),
        Expr::RecordUpdate { .. } | Expr::RecordField { .. } => {
            // Side-effect: may throw (e.g. expr not a record)
            false
        }
        Expr::RecordIndex { .. } => true,

        Expr::NativeRecord { fields, .. } => fields
            .iter()
            .all(|(_key, value)| has_no_effect(def_fb, value)),
        Expr::NativeRecordUpdate { .. } | Expr::NativeRecordField { .. } => {
            // Side-effect: may throw (e.g. expr not a record)
            false
        }

        Expr::UnaryOp { .. } | Expr::BinaryOp { .. } => {
            // Side-effect: may throw
            false
        }
        Expr::MacroCall {
            expansion,
            args: _,
            macro_def: _,
            macro_name: _,
        } => has_no_effect(def_fb, expansion),
        Expr::Call { .. } | Expr::Receive { .. } => false,

        Expr::Block { exprs } => exprs.iter().all(|stmt| has_no_effect(def_fb, stmt)),
        Expr::Catch { expr } => has_no_effect(def_fb, expr),
        Expr::Try { of_clauses, .. } if !of_clauses.is_empty() => {
            // Side-effect: may fail to match
            false
        }
        Expr::Try { exprs, after, .. } => {
            // NB. if exprs has no effect, nothing can throw
            // so there can be no match errors in catch_clauses
            exprs.iter().all(|stmt| has_no_effect(def_fb, stmt))
                && after.iter().all(|stmt| has_no_effect(def_fb, stmt))
        }
        Expr::Match { .. } | Expr::If { .. } | Expr::Case { .. } | Expr::Maybe { .. } => {
            // Side-effects:
            //   - binding of variables
            //   - may fail to match
            false
        }
        Expr::Paren { .. } => false,
    }
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::BinaryStringToSigil)
            .disable(DiagnosticCode::UnspecificInclude)
            .disable(DiagnosticCode::NoCatch)
            .disable(DiagnosticCode::RedundantFunWrapper);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn check_removes_comma_and_other_stuff() {
        check_fix(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                ~ok,
                do_something_else(),
                ok.
            do_something() -> ok.
            "#,
            expect![[r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                do_something_else(),
                ok.
            do_something() -> ok.
            "#]],
        );
    }

    #[test]
    fn remove_useless_atom() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                ok,
            %%  ^^ 💡 warning: W0006: this statement has no effect
                do_something_else(),
                bar,
            %%  ^^^ 💡 warning: W0006: this statement has no effect
                ok.
            do_something() -> ok.
            "#,
        );
    }

    #[test]
    fn remove_useless_var() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                X = 42,
                X,
            %%  ^ 💡 warning: W0006: this statement has no effect
                ok.
            "#,
        );
    }

    #[test]
    fn remove_useless_literals() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                42,
            %%  ^^ 💡 warning: W0006: this statement has no effect
                41.9999,
            %%  ^^^^^^^ 💡 warning: W0006: this statement has no effect
                do_something_else(),
                "foo",
            %%  ^^^^^ 💡 warning: W0006: this statement has no effect
                <<"foo">>,
            %%  ^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                'A',
            %%  ^^^ 💡 warning: W0006: this statement has no effect
                ok.
            do_something() -> 42.
            "#,
        );
    }

    #[test]
    fn remove_useless_lambda() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            test_foo(_Config) ->
                do_something(),
                fun() -> do_something() end,
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                F = fun() -> do_something() end,
                F(),
                fun do_something/0,
            %%  ^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                fun erlang:length/1,
            %%  ^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                ok.
            do_something() -> 42.
            //- /src/erlang.erl
            -module(erlang).
            -export([length/1]).
            length(_) -> 42.
            "#,
        );
    }

    #[test]
    fn remove_under_parens() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                (do_something()),
                (blah),
            %%  ^^^^^^ 💡 warning: W0006: this statement has no effect
                ok.
            do_something() -> (abc).
            "#,
        );
    }

    #[test]
    fn remove_useless_blocks() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                begin abc, blah, ("foo") end,
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
            %%        ^^^ 💡 warning: W0006: this statement has no effect
            %%             ^^^^ 💡 warning: W0006: this statement has no effect
                begin
                  do_something(),
                  blah,
              %%  ^^^^ 💡 warning: W0006: this statement has no effect
                  ok
                end,
                ok.
            do_something() -> (abc).
            "#,
        );
    }

    #[test]
    fn remove_useless_lists() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                [42, blah, ("foo")],
            %%  ^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                [42, do_something(), blah],
                [],
            %%  ^^ 💡 warning: W0006: this statement has no effect
                ok.
            do_something() -> [].
            "#,
        );
    }

    #[test]
    fn remove_useless_tuples() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                {42, [blah], {"foo"}},
            %%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                {42, do_something(), blah},
                {},
            %%  ^^ 💡 warning: W0006: this statement has no effect
                ok.
            do_something() -> [].
            "#,
        );
    }

    #[test]
    fn remove_useless_record_operations() {
        check_diagnostics(
            r#"
            -module(main).
            -record(person, {name, age}).
            test_foo(P) ->
                do_something(),
                #person{name="Bob", age=42},
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                #person{name=get_name(), age=42},
                P#person{name="Alice"},
                #person.name,
            %%  ^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                P#person.name,
                ok.
            get_name() -> "bob".
            "#,
        );
    }

    #[test]
    fn remove_useless_map_operations() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(P) ->
                do_something(),
                #{name => "Bob", age => 42},
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                #{name => get_name(), age => 42},
                #{get_key() => "Bob", age => 42},
                P#{name=>"Alice"},
                ok.
            get_name() -> "bob".
            get_key() -> name.
            "#,
        );
    }

    #[test]
    fn remove_useless_try_catch_operations() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_P) ->
                catch do_something(),
                catch ok,
            %%  ^^^^^^^^ 💡 warning: W0006: this statement has no effect
                try does, nothing catch _ -> do_stuff() end,
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                %%  ^^^^ 💡 warning: W0006: this statement has no effect
                try
                    does_nothing
                of _ -> ok
                catch _ -> not_ok
                end,
                try
                    do_something()
                after
                    ok
                end,
                try does, nothing after blah, ok end,
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0006: this statement has no effect
                %%  ^^^^ 💡 warning: W0006: this statement has no effect
                                    %%  ^^^^ 💡 warning: W0006: this statement has no effect
                try
                    does, nothing
                %%  ^^^^ 💡 warning: W0006: this statement has no effect
                of _ -> foo, bar
                    %%  ^^^ 💡 warning: W0006: this statement has no effect
                catch
                  _ -> 42, not_ok
                   %%  ^^ 💡 warning: W0006: this statement has no effect
                after
                  [1,2,3],
              %%  ^^^^^^^ 💡 warning: W0006: this statement has no effect
                  ok
                end,
                ok.
            "#,
        );
    }

    #[test]
    fn ignore_stuff_introduced_from_macros() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
            -define(included_noop(X), noop).

//- /src/foo.erl
            -module(foo).
            -include("foo.hrl").
            -define(noop(X), another_noop).
            -define(also_noop, yet_another_noop).

            blah() ->
                 noop,
             %%  ^^^^ 💡 warning: W0006: this statement has no effect
                 do_something(),
                 ?included_noop(42),
                 do_something(),
                 ?noop(42),
                 ?also_noop,
                 ok.
        "#,
        )
    }

    #[test]
    fn ignore_multistatement_macro_expansion() {
        // A same-file macro whose expansion is a multi-statement block must
        // not produce a diagnostic pointing inside the macro definition.
        // The HIR-only walker stops at any `in_macro` ancestor, so neither
        // `a` nor `b` inside the expansion is considered a user statement.
        check_diagnostics(
            r#"
            -module(main).
            -define(two_stmts, begin a, b end).

            test() ->
                ?two_stmts,
                do_something(),
                ok.
            do_something() -> ok.
            "#,
        )
    }

    #[test]
    fn maybe_block_statements() {
        // In a `maybe` block, an effect-free `Expr` statement is only
        // flagged when the next entry is also an `Expr` — never when it
        // is a `Cond` (`Pat ?= Expr`). With a `Cond` as the next entry
        // the deletion range start..start would span the cond's `?=`
        // pattern and produce an incorrect fix, so we conservatively
        // skip the diagnostic.
        check_diagnostics(
            r#"
            -module(main).

            ok_a() -> ok.
            ok_b() -> ok.

            test() ->
                maybe
                    a,
                %%  ^ 💡 warning: W0006: this statement has no effect
                    b,
                    {ok, AA} ?= ok_a(),
                    {ok, BB} ?= ok_b(),
                    {AA, BB}
                end.
            "#,
        )
    }
}
