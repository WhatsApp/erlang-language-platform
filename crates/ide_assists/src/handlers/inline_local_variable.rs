/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::ast;
use elp_syntax::ast::AstNode;
use elp_syntax::TextRange;
use hir::db::DefDatabase;
use hir::InFile;
use hir::Semantic;

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;
use crate::helpers;
use crate::helpers::extend_delete_range;

// Assist: inline_local_variable
//
// Replaces the occurrence of a variable with its definition
//
// ```
// A = 3 + B,
// foo(A).
// ```
// ->
// ```
// foo(3 + B).
// ```
pub(crate) fn inline_local_variable(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let InlineData {
        lhs_var,
        match_expr,
        delete_definition,
        target,
        references,
    } = if let Some(data) = ctx
        .find_node_at_offset::<ast::MatchExpr>()
        .and_then(|match_expr| inline_variable_definition(ctx, match_expr))
    {
        Some(data)
    } else {
        ctx.find_node_at_offset::<ast::Var>()
            .and_then(|var| inline_usage(ctx, var))
    }?;

    if !is_safe(ctx, lhs_var, &match_expr, &references) {
        return None;
    }

    let rhs = match_expr.rhs()?;

    acc.add(
        AssistId("inline_local_variable", AssistKind::RefactorInline),
        "Inline variable",
        None,
        target,
        None,
        move |builder| {
            let delete_range = delete_definition.then(|| extend_delete_range(match_expr.syntax()));

            let init_str = rhs.syntax().text().to_string();
            let init_in_paren = format!("({})", &init_str);
            if let Some(range) = delete_range {
                builder.delete(range);
            }
            references
                .into_iter()
                .filter_map(|v| helpers::parens_needed(&rhs, &v))
                .for_each(|(range, should_wrap)| {
                    let replacement = if should_wrap {
                        &init_in_paren
                    } else {
                        &init_str
                    };
                    builder.replace(range, replacement.clone())
                })
        },
    )
}

struct InlineData {
    lhs_var: ast::Var,
    match_expr: ast::MatchExpr,
    delete_definition: bool,
    target: TextRange,
    references: Vec<ast::Var>,
}

fn inline_variable_definition(
    ctx: &AssistContext,
    match_expr: ast::MatchExpr,
) -> Option<InlineData> {
    let lhs_var = match match_expr.lhs()? {
        ast::Expr::ExprMax(ast::ExprMax::Var(v)) => v,
        _ => return None,
    };

    // The finding of the candidate is noisy, may have some unrelated
    // ancestor, check that the ranges are sane.
    if !lhs_var
        .syntax()
        .text_range()
        .contains_range(ctx.selection_trimmed())
    {
        return None;
    }

    let db = ctx.db();
    let references = find_local_usages(&ctx.sema, db, InFile::new(ctx.file_id(), &lhs_var))?;

    let target = lhs_var.syntax().text_range();
    Some(InlineData {
        lhs_var,
        match_expr,
        delete_definition: true,
        target,
        references,
    })
}

/// Inline the single reference indicated.  Only remove the variable
/// definition if there are no other usages.
fn inline_usage(ctx: &AssistContext, var: ast::Var) -> Option<InlineData> {
    // The finding of the candidate is noisy, may have some unrelated
    // ancestor, check that the ranges are sane.
    if !var
        .syntax()
        .text_range()
        .contains_range(ctx.selection_trimmed())
    {
        return None;
    }
    let db = ctx.db();
    let var_defs = ctx
        .sema
        .to_def(InFile::new(ctx.file_id(), &var))?
        .to_reference()?;
    let lhs_var = if var_defs.len() == 1 {
        var_defs[0].source(db.upcast())
    } else {
        return None;
    };
    let match_expr = ast::MatchExpr::cast(lhs_var.syntax().parent()?)?;

    // Are trying to inline ourselves?
    if lhs_var.syntax().text_range() == var.syntax().text_range() {
        return None;
    }

    let mut references = find_local_usages(&ctx.sema, db, InFile::new(ctx.file_id(), &lhs_var))?;
    let delete_definition = references.len() == 1;
    references.retain(|fref| fref.syntax().text_range() == var.syntax().text_range());

    let target = lhs_var.syntax().text_range();
    Some(InlineData {
        lhs_var,
        match_expr,
        delete_definition,
        target,
        references,
    })
}

/// Find all other variables within the function clause that resolve
/// to the one given, but only if there is a single
/// resolution. Variables having multiple binding sites, as arising
/// from case clauses, cannot be inlined.
fn find_local_usages(
    sema: &Semantic,
    db: &dyn DefDatabase,
    var: InFile<&ast::Var>,
) -> Option<Vec<ast::Var>> {
    // TODO: replace this function with the appropriate one when the
    // highlight usages feature exists. T128835148
    let var_range = var.value.syntax().text_range();
    let clause = var
        .value
        .syntax()
        .ancestors()
        .find_map(ast::FunctionClause::cast)?;

    let vars: Vec<_> = clause
        .syntax()
        .descendants()
        .filter_map(ast::Var::cast)
        .filter_map(|v| {
            let ds = sema.to_def(InFile::new(var.file_id, &v))?.to_reference()?;
            if ds.len() == 1 {
                // We have resolved a candidate Var.
                // Check that it resolves to the one we are currently attempting to inline
                // And that we have not found ourselves.
                if ds[0].source(db.upcast()).syntax().text_range() == var_range
                    && v.syntax().text_range() != var_range
                {
                    Some(v)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();

    if vars.is_empty() { None } else { Some(vars) }
}

/// Check that all free variables defined in the RHS of the `MatchExpr`
/// have the same bindings in the target location, in `references`.
fn is_safe(
    ctx: &AssistContext,
    lhs_var: ast::Var,
    match_expr: &ast::MatchExpr,
    references: &[ast::Var],
) -> bool {
    // We care about two kinds of variables external to the
    // `match_expr` rhs.
    // 1. Ones that are free in the rhs expression and already bound
    //    in the surrounding context. These must be bound to the same
    //    value in the new location.
    // 2. Ones that are at the top level in the rhs expression, and
    //    are bound in the expression. These must be unbound in the
    //    new location.

    // Use closure to allow use of `?` operator
    if let Some(r) = || -> Option<bool> {
        let rhs = match_expr.rhs()?;
        let rhs_vars = ctx.sema.free_vars_ast(ctx.file_id(), &rhs)?;

        let (resolver_orig, scope_orig) = ctx.sema.scope_for(InFile {
            file_id: ctx.file_id(),
            value: &lhs_var,
        })?;

        if references.iter().all(|var: &ast::Var| {
            let var_in = InFile {
                file_id: ctx.file_id(),
                value: var,
            };
            if let Some((resolver_var, scope_var)) = ctx.sema.scope_for(var_in) {
                // XXX_orig is where the RHS was originally defined
                // XXX_var is where we want to inline it

                // Check that all of `rhs_free_vars` have the same definition in both places
                let free_ok = rhs_vars.free.iter().all(|(var, _defs)| {
                    resolver_orig.resolve_var_in_scope(var, scope_orig)
                        == resolver_var.resolve_var_in_scope(var, scope_var)
                });

                // Check that all of `rhs_bound_vars` are unbound in the new place
                let bound_ok = rhs_vars.bound.iter().all(|(var, _defs)| {
                    resolver_var.resolve_var_in_scope(var, scope_var).is_none()
                });
                free_ok && bound_ok
            } else {
                false
            }
        }) {
            Some(true)
        } else {
            Some(false)
        }
    }() {
        r
    } else {
        false
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn test_definition1() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = 3 + B,
  foo(A).
"#,
            expect![[r#"
                bar() ->
                  foo(3 + B).
            "#]],
        )
    }

    #[test]
    fn test_definition2() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
foo() ->
    ~VarName = (1 + 2),
    VarName * 4.
"#,
            expect![[r#"
                foo() ->
                    (1 + 2) * 4.
            "#]],
        )
    }

    #[test]
    fn test_definition_case() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar(X) ->
  ~A = 3 + B,
  case X of
    A -> none
  end.
"#,
            expect![[r#"
                bar(X) ->
                  case X of
                    3 + B -> none
                  end.
            "#]],
        )
    }

    #[test]
    fn test_shadowed_variable_free_vars() {
        check_assist_not_applicable(
            inline_local_variable,
            r#"
bar() ->
  B = 3,
  ~A = begin
         C = 2,
         case B of
           3 -> 6;
           _ -> C
         end
        end,
  (fun() ->
    C = 5,
    foo(A) % Not applicable, C is already bound in the new location
  end)().
"#,
        )
    }

    #[test]
    fn test_usage_case() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
baz(X) ->
    V = 3,
    A = case X of
            ~V -> X + 4;
            _ -> X
        end,
    A.
"#,
            expect![[r#"
                baz(X) ->
                    A = case X of
                            3 -> X + 4;
                            _ -> X
                        end,
                    A.
            "#]],
        )
    }

    #[test]
    fn test_inline_in_case1() {
        check_assist_not_applicable(
            inline_local_variable,
            r#"
        bar(XX) ->
        case XX of
            1 ->
                Z = 3;
            2 ->
                ~Z = 4
        end,
        foo(Z).

        "#,
        );
    }

    #[test]
    fn test_usage_one_only() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  A = 3 + B,
  foo(~A).
"#,
            expect![[r#"
                bar() ->
                  foo(3 + B).
            "#]],
        )
    }

    #[test]
    fn test_usage_multiple() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  A = 3 + B,
  foo(~A),
  bar(A).
"#,
            expect![[r#"
                bar() ->
                  A = 3 + B,
                  foo(3 + B),
                  bar(A).
            "#]],
        )
    }

    #[test]
    fn test_inline_usage_in_case1() {
        check_assist_not_applicable(
            inline_local_variable,
            r#"
        bar() ->
        case rand:uniform() of
            1 ->
                Z = 3;
            2 ->
                Z = 4
        end,
        foo(~Z).

        "#,
        );
    }

    #[test]
    fn test_parens_needed1() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = 3 + B,
  X = A * 3.
"#,
            expect![[r#"
                bar() ->
                  X = (3 + B) * 3.
            "#]],
        )
    }

    #[test]
    fn test_parens_needed2() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = baz(4),
  X = A * 3.
"#,
            expect![[r#"
                bar() ->
                  X = baz(4) * 3.
            "#]],
        )
    }

    #[test]
    fn test_parens_needed3() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = B#{ foo := 3},
  X = A * 3.
"#,
            expect![[r##"
                bar() ->
                  X = B#{ foo := 3} * 3.
            "##]],
        )
    }

    #[test]
    fn test_parens_needed4() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = baz,
  X = A * 3.
"#,
            expect![[r##"
                bar() ->
                  X = baz * 3.
            "##]],
        )
    }

    #[test]
    fn test_parens_needed_parent1() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = B + 3,
  foo(A,4).
"#,
            expect![[r#"
                bar() ->
                  foo(B + 3,4).
            "#]],
        )
    }

    #[test]
    fn test_parens_needed_parent2() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = B + 3,
  [A|A],
  Y = A,
  catch A,
  begin
    A,
    Y = 6
  end,
  A.
"#,
            expect![[r#"
                bar() ->
                  [B + 3|B + 3],
                  Y = B + 3,
                  catch B + 3,
                  begin
                    B + 3,
                    Y = 6
                  end,
                  B + 3.
            "#]],
        )
    }

    #[test]
    fn test_surrounding_commas1() {
        check_assist(
            inline_local_variable,
            "Inline variable",
            r#"
bar() ->
  ~A = 3 + B ,
  C = 5,
  bar(A).
"#,
            expect![[r#"
                bar() ->
                  C = 5,
                  bar(3 + B).
            "#]],
        )
    }
}
