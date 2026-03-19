/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint/fix: trivial_match
//!
//! Return a diagnostic if a match will trivially always succeed and offer to
//! remove the lhs as a fix.

use std::collections::HashMap;

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::BinarySeg;
use hir::Expr;
use hir::ExprId;
use hir::FunctionClauseDef;
use hir::InFunctionClauseBody;
use hir::Literal;
use hir::Pat;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::codemod_helpers::is_only_place_where_var_is_defined;
use crate::codemod_helpers::var_has_no_references;
use crate::codemod_helpers::var_name_starts_with_underscore;
use crate::diagnostics::Category;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

pub(crate) struct TrivialMatchLinter;

impl Linter for TrivialMatchLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::TrivialMatch
    }

    fn description(&self) -> &'static str {
        "match is redundant"
    }

    fn is_experimental(&self) -> bool {
        // TODO: disable this check when T151727890 and T151605845 are resolved
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Context {
    full_range: TextRange,
    replacement: Option<String>,
}

impl GenericLinter for TrivialMatchLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let mut res = Vec::new();
        sema.def_map(file_id)
            .get_function_clauses()
            .for_each(|(_, def)| {
                if def.file.file_id == file_id {
                    let in_clause = def.in_clause(sema, def);
                    let body_map = in_clause.get_body_map();
                    let source_file = sema.parse(def.file.file_id);

                    in_clause.fold_clause(
                        Strategy {
                            macros: MacroStrategy::Expand,
                            parens: ParenStrategy::InvisibleParens,
                        },
                        (),
                        &mut |_acc, ctx| {
                            if let AnyExpr::Expr(Expr::Match { lhs, rhs }) = ctx.item {
                                let rhs = &rhs.clone();
                                if matches_trivially(sema, &in_clause, &lhs, rhs) {
                                    let maybe_lhs_range =
                                        &in_clause.range_for_any(AnyExprId::Pat(lhs));
                                    let maybe_full_range = &in_clause.range_for_any(ctx.item_id);
                                    if let (Some(lhs_range), Some(full_range)) =
                                        (maybe_lhs_range, maybe_full_range)
                                    {
                                        let rhs_ast =
                                            body_map.expr(*rhs).and_then(|infile_ast_ptr| {
                                                infile_ast_ptr.to_node(&source_file)
                                            });
                                        let file_id = def.file.file_id;
                                        if lhs_range.file_id == file_id
                                            && full_range.file_id == file_id
                                        {
                                            let replacement = rhs_ast
                                                .map(|replacement_ast| replacement_ast.to_string());
                                            res.push(GenericLinterMatchContext {
                                                range: FileRange {
                                                    file_id: lhs_range.file_id,
                                                    range: lhs_range.range,
                                                },
                                                context: Context {
                                                    full_range: full_range.range,
                                                    replacement,
                                                },
                                            });
                                        }
                                    }
                                }
                            }
                        },
                    );
                }
            });
        Some(res)
    }

    fn fixes(
        &self,
        context: &Context,
        _range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let replacement_str = context.replacement.as_ref()?;
        let mut edit_builder = TextEdit::builder();
        edit_builder.replace(context.full_range, replacement_str.clone());
        let edit = edit_builder.finish();

        Some(vec![fix(
            "remove_redundant_match",
            "Remove match",
            SourceChange::from_text_edit(file_id, edit),
            context.full_range,
        )])
    }

    fn add_categories(&self, _context: &Context) -> Vec<Category> {
        vec![Category::SimplificationRule]
    }
}

pub(crate) static LINTER: TrivialMatchLinter = TrivialMatchLinter;

fn matches_trivially(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionClauseDef>,
    pat_id: &PatId,
    expr_id: &ExprId,
) -> bool {
    let pat = &in_clause[*pat_id];
    let expr = &in_clause[*expr_id];
    match pat {
        Pat::Missing => false,

        Pat::Literal(l) => match expr {
            Expr::Literal(r) => l == r,
            _ => false,
        },
        Pat::Var(l) => {
            let in_clause_var = in_clause.with_value(AnyExprId::Pat(*pat_id));

            if !var_name_starts_with_underscore(sema.db.upcast(), l)
                && is_only_place_where_var_is_defined(sema, &in_clause_var)
                && var_has_no_references(sema, &in_clause_var)
            {
                // RHS defines a variable, so this will always match. Moreover, the the variable is
                // never used, so we can safely remover it.
                return true;
            }

            match expr {
                Expr::Var(r) => l == r,
                _ => false,
            }
        }

        Pat::Match { .. } => false,

        Pat::Tuple { pats } => match expr {
            Expr::Tuple { exprs } if pats.len() == exprs.len() => pats
                .iter()
                .zip(exprs.iter())
                .all(|(p, e)| matches_trivially(sema, in_clause, p, e)),
            _ => false,
        },

        Pat::List { pats, tail: None } => match expr {
            Expr::List { exprs, tail: None } if pats.len() == exprs.len() => pats
                .iter()
                .zip(exprs.iter())
                .all(|(p, e)| matches_trivially(sema, in_clause, p, e)),
            _ => false,
        },
        Pat::List { .. } => false,

        Pat::Record {
            name: pat_name,
            fields: pat_fields,
        } => match expr {
            Expr::Record {
                name: expr_name,
                fields: expr_fields,
            } => match {} {
                _ if pat_name != expr_name => false,
                _ => {
                    let pat_fields_map = pat_fields.iter().copied().collect::<HashMap<_, _>>();
                    let expr_fields_map = expr_fields.iter().copied().collect::<HashMap<_, _>>();
                    pat_fields_map.iter().all(|(field, pat_val)| {
                        if let Some(expr_val) = expr_fields_map.get(field) {
                            matches_trivially(sema, in_clause, pat_val, expr_val)
                        } else {
                            false
                        }
                    })
                }
            },
            _ => false,
        },
        Pat::RecordIndex { .. } => false,

        Pat::Map { fields: pat_fields } => match expr {
            Expr::Map {
                fields: expr_fields,
            } => {
                let pat_fields_map = pat_fields
                    .iter()
                    .filter_map(|(field, val)| {
                        let lit = as_literal(in_clause, field)?;
                        Some((lit, val))
                    })
                    .collect::<HashMap<_, _>>();

                // We only handle maps with literals as keys, so ensure no
                // key got lost in the translation
                if pat_fields_map.len() < pat_fields.len() {
                    false
                } else {
                    let expr_fields_map = expr_fields
                        .iter()
                        .filter_map(|(field, val)| {
                            let lit = as_literal(in_clause, field)?;
                            Some((lit, val))
                        })
                        .collect::<HashMap<_, _>>();

                    pat_fields_map.iter().all(|(field, pat_val)| {
                        if let Some(expr_val) = expr_fields_map.get(field) {
                            matches_trivially(sema, in_clause, pat_val, expr_val)
                        } else {
                            false
                        }
                    })
                }
            }
            _ => false,
        },

        Pat::Binary { segs: pat_segs } => match expr {
            Expr::Binary { segs: expr_segs } => {
                let trivial_seg = BinarySeg {
                    elem: {},
                    size: None,
                    tys: vec![],
                    unit: None,
                };
                pat_segs
                    .iter()
                    .zip(expr_segs.iter())
                    .all(|(pat_seg, expr_seg)| {
                        pat_seg.with_value(()) == trivial_seg
                            && expr_seg.with_value(()) == trivial_seg
                            && matches_trivially(sema, in_clause, &pat_seg.elem, &expr_seg.elem)
                    })
            }

            _ => false,
        },

        Pat::UnaryOp { .. } | Pat::BinaryOp { .. } => false,
        Pat::MacroCall {
            expansion,
            args: _,
            macro_def: _,
            macro_name: _,
        } => matches_trivially(sema, in_clause, expansion, expr_id),
        Pat::Paren { pat } => matches_trivially(sema, in_clause, pat, expr_id),
    }
}

fn as_literal(
    def_fb: &InFunctionClauseBody<&FunctionClauseDef>,
    expr_id: &ExprId,
) -> Option<Literal> {
    let expr = &def_fb[*expr_id];
    match expr {
        Expr::Literal(lit) => Some(lit.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn can_fix() {
        check_fix(
            r#"
            -module(main).

            do_foo() ->
              X = ~42 = 42,
              ok.
            "#,
            expect![[r#"
            -module(main).

            do_foo() ->
              X = 42,
              ok.
            "#]],
        );
        check_fix(
            r#"
            -module(main).

            do_foo() ->
              X = ~foo(bar),
              ok.
            "#,
            expect![[r#"
            -module(main).

            do_foo() ->
              foo(bar),
              ok.
            "#]],
        )
    }

    #[test]
    fn trivial_lit_matches() {
        check_diagnostics(
            r#"
            -module(main).

            do_foo() ->
                42 = 42,
            %%  ^^ 💡 warning: W0007: match is redundant
                42 = 43,
                "blah" = "blah",
            %%  ^^^^^^ 💡 warning: W0007: match is redundant
                "blah" = "bleh",
                'x' = 'x',
            %%  ^^^ 💡 warning: W0007: match is redundant
                'x' = 'X',
                true = true,
            %%  ^^^^ 💡 warning: W0007: match is redundant
                true = false,
                ok.
            "#,
        )
    }

    #[test]
    fn trivial_var_matches() {
        check_diagnostics(
            r#"
            -module(main).

            do_foo() ->
                X = 42,
                Y = 42,
                X = X,
            %%  ^ 💡 warning: W0007: match is redundant
                X = Y,
                {Z} = {Y},
            %%  ^^^ 💡 warning: W0007: match is redundant
                [W, ok] = [ok, ok],
            %%  ^^^^^^^ 💡 warning: W0007: match is redundant
                [_W, ok] = [ok, ok],
                ok.
            "#,
        )
    }

    #[test]
    fn trivial_binary_matches() {
        check_diagnostics(
            r#"
            -module(main).

            do_foo() ->
                X = 42,
                <<"foo", 42>> = <<"foo", 42>>,
            %%  ^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                <<"foo", X>> = <<"foo", X>>,
            %%  ^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                <<"foo", Y>> = <<"foo", 42>>,
                Y.
            "#,
        )
    }
    #[test]
    fn trivial_tuple_matches() {
        check_diagnostics(
            r#"
            -module(main).

            do_foo() ->
                X = 42,
                {X, "foo", {foo, bar}} = {X, "foo", {foo, bar}},
            %%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                {X, foo} = {X, bar},
                {X, "foo", {foo, bar}} = {X, "foo", {foo, pub}},
                {X, "foo", {foo, bar}} = {X, "foo", {foo, bar, hey}},
                {} = {},
            %%  ^^ 💡 warning: W0007: match is redundant
                ok.
            "#,
        )
    }

    #[test]
    fn trivial_list_matches() {
        check_diagnostics(
            r#"
            -module(main).

            do_foo() ->
                X = 42,
                [X, ["foo"], [foo, bar]] = [X, ["foo"], [foo, bar]],
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                [X, foo] = [X, bar],
                [X, "foo", [foo, bar]] = [X, "foo", [foo, pub]],
                [X, "foo", [foo, bar]] = [X, "foo", [foo, bar, hey]],
                [] = [],
            %%  ^^ 💡 warning: W0007: match is redundant
                ok.
            "#,
        )
    }

    #[test]
    fn trivial_record_matches() {
        check_diagnostics(
            r#"
            -module(main).

            -record(person, {name, age}).

            do_foo() ->
                #person{name = "Joe", age = 42} = #person{age = 42, name = "Joe"},
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                #person{name = "Joe", age = 43} = #person{age = 42, name = "Joe"},
                #person{name = "Joe"} = #person{age = 42, name = "Joe"},
            %%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                #person{age = 42} = #person{age = 42, name = "Joe"},
            %%  ^^^^^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                ok.
            "#,
        )
    }

    #[test]
    fn trivial_maps_matches() {
        check_diagnostics(
            r#"
            -module(main).

            do_foo() ->
                #{name := "Joe", age := 42} = #{age => 42, name => "Joe"},
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                #{name := "Joe", age := 43} = #{age => 42, name => "Joe"},
                #{name := "Joe"} = #{age => 42, name => "Joe"},
            %%  ^^^^^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                #{age := 42} = #{age => 42, name => "Joe"},
            %%  ^^^^^^^^^^^^ 💡 warning: W0007: match is redundant
                ok.
            "#,
        )
    }

    #[test]
    fn macro_arg_uses_variable_no_expansion() {
        check_diagnostics(
            r#"
            -module(main).

            ok() -> ok.
            %% -define(
            foo() ->
                A = ok(),
                ?assertMatch(A, ok()).
            "#,
        )
    }

    #[test]
    fn macro_arg_uses_variable_with_expansion() {
        check_diagnostics(
            r#"
            -module(main).

            ok() -> ok.
            -define(assertMatch(X,Y), {X,Y}).

            foo() ->
                A = ok(),
                ?assertMatch(A, ok()).
            "#,
        )
    }
}
