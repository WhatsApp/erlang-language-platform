/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistUserInput;
use elp_ide_db::assists::AssistUserInputType;
use elp_syntax::AstNode;
use elp_syntax::SyntaxNode;
use elp_syntax::ast;
use elp_syntax::ast::edit::IndentLevel;
use fxhash::FxHashSet;
use hir::ScopeAnalysis;
use hir::Var;
use hir::resolver::Resolution;
use stdx::format_to;

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;
use crate::helpers::freshen_function_name;
use crate::helpers::make_function_name;
use crate::helpers::node_to_insert_after;

// Assist: case_to_function_clauses
//
// Extract a `case` expression into a new function with pattern-matching clauses.
//
// ```
// foo(X) ->
//     case X of
//         {ok, V} -> V;
//         error -> default
//     end.
// ```
// ->
// ```
// foo(X) ->
//     fun_name(X).
//
// fun_name({ok, V}) ->
//     V;
// fun_name(error) ->
//     default.
// ```
pub(crate) fn case_to_function_clauses(acc: &mut Assists, ctx: &AssistContext<'_>) -> Option<()> {
    let case_expr = ctx.find_node_at_offset::<ast::CaseExpr>()?;
    let scrutinee = case_expr.expr()?;

    // Collect all clauses, bail if any is a macro
    let clauses: Vec<ast::CrClause> = case_expr
        .clauses()
        .map(|clause_or_macro| match clause_or_macro {
            ast::CrClauseOrMacro::CrClause(clause) => Some(clause),
            ast::CrClauseOrMacro::MacroCallExpr(_) => None,
        })
        .collect::<Option<Vec<_>>>()?;

    if clauses.is_empty() {
        return None;
    }

    // Check that each clause has a pattern and body
    for clause in &clauses {
        clause.pat()?;
        clause.body()?;
    }

    // We need to be inside a function to extract
    ctx.sema
        .find_enclosing_function(ctx.file_id(), case_expr.syntax())?;

    let insert_after = node_to_insert_after(case_expr.syntax())?;
    let target_range = case_expr.syntax().text_range();

    acc.add(
        AssistId(
            "case_to_function_clauses",
            crate::AssistKind::RefactorExtract,
        ),
        "Extract case to function clauses",
        None,
        target_range,
        Some(AssistUserInput {
            input_type: AssistUserInputType::Atom,
            prompt: None,
            value: make_function_name(ctx),
            task_id: None,
        }),
        move |builder| {
            // Analyze free variables in the case expression
            let mut analyzer = ScopeAnalysis::new();
            analyzer.walk_ast_expr(
                &ctx.sema,
                ctx.file_id(),
                ast::Expr::ExprMax(ast::ExprMax::CaseExpr(case_expr.clone())),
            );

            // Analyze free variables in just the scrutinee
            let mut scrutinee_analyzer = ScopeAnalysis::new();
            scrutinee_analyzer.walk_ast_expr(&ctx.sema, ctx.file_id(), scrutinee.clone());
            let scrutinee_vars: FxHashSet<Var> = scrutinee_analyzer
                .free
                .iter()
                .map(|(var, _)| *var)
                .collect();

            // Analyze free variables in clause bodies only (not patterns/guards)
            // to detect if a scrutinee variable is also used inside clause bodies
            let mut body_analyzer = ScopeAnalysis::new();
            for clause in &clauses {
                if let Some(body) = clause.body() {
                    for expr in body.exprs() {
                        body_analyzer.walk_ast_expr(&ctx.sema, ctx.file_id(), expr);
                    }
                }
            }
            let body_free_vars: FxHashSet<Var> =
                body_analyzer.free.iter().map(|(var, _)| *var).collect();

            // Free variables become additional parameters (beyond the scrutinee).
            // Exclude scrutinee variables ONLY if they are not also used in
            // clause bodies. If a variable appears in both the scrutinee and
            // a clause body, it must be passed as an extra parameter.
            let free_vars: Vec<Var> = analyzer
                .free
                .into_iter()
                .filter(|(var, _)| !scrutinee_vars.contains(var) || body_free_vars.contains(var))
                .map(|(var, _)| var)
                .collect();

            // Compute outliving locals (variables bound in the case that are used afterward)
            let outliving_locals =
                compute_outliving_locals(ctx, case_expr.syntax(), &analyzer.bound);

            let name = freshen_function_name(
                ctx,
                ctx.user_input_or(|| make_function_name(ctx)),
                (1 + free_vars.len()) as u32, // scrutinee + free vars
            );

            // Build the call site replacement
            let call_text = make_call_text(ctx, &name, &scrutinee, &free_vars, &outliving_locals);

            // Build the new function definition
            let new_indent = IndentLevel::from_node(&insert_after);
            let fn_def = make_function_def(
                ctx,
                &name,
                &clauses,
                &free_vars,
                &outliving_locals,
                new_indent,
            );

            builder.replace(target_range, call_text);
            let insert_offset = insert_after.text_range().end();

            match ctx.config.snippet_cap {
                Some(cap) => builder.insert_snippet(cap, insert_offset, fn_def),
                None => builder.insert(insert_offset, fn_def),
            };
        },
    )
}

/// Build the call expression text to replace the case expression
fn make_call_text(
    ctx: &AssistContext<'_>,
    name: &str,
    scrutinee: &ast::Expr,
    free_vars: &[Var],
    outliving_locals: &[Var],
) -> String {
    let mut args = vec![scrutinee.syntax().text().to_string()];
    for var in free_vars {
        args.push(ctx.db().lookup_var(*var).to_string());
    }

    let args_str = args.join(", ");

    let mut buf = String::new();
    match outliving_locals {
        [] => {}
        [local] => {
            format_to!(buf, "{} = ", local.as_string(ctx.db().upcast()));
        }
        vars => {
            buf.push('{');
            let bindings: Vec<_> = vars
                .iter()
                .map(|v| v.as_string(ctx.db().upcast()))
                .collect();
            buf.push_str(&bindings.join(", "));
            buf.push_str("} = ");
        }
    }

    format_to!(buf, "{}({})", name, args_str);
    buf
}

/// Build the new function definition text with multiple clauses
fn make_function_def(
    ctx: &AssistContext<'_>,
    name: &str,
    clauses: &[ast::CrClause],
    free_vars: &[Var],
    outliving_locals: &[Var],
    new_indent: IndentLevel,
) -> String {
    let mut fn_def = String::new();
    let free_var_names: Vec<String> = free_vars
        .iter()
        .map(|v| ctx.db().lookup_var(*v).to_string())
        .collect();

    let use_snippet = ctx.config.snippet_cap.is_some();

    for (i, clause) in clauses.iter().enumerate() {
        let is_first = i == 0;
        let is_last = i == clauses.len() - 1;

        let pat_text = clause
            .pat()
            .map(|p| p.syntax().text().to_string())
            .unwrap_or_default();

        // Build argument list: pattern + free vars
        let mut args = vec![pat_text];
        args.extend(free_var_names.clone());
        let args_str = args.join(", ");

        // Guard
        let guard_text = clause
            .guard()
            .map(|g| format!(" when {}", g.syntax().text()));

        // Body — extract just the expression texts, not the `->` token
        let body_text = clause
            .body()
            .map(|b| {
                let exprs: Vec<String> = b.exprs().map(|e| e.syntax().text().to_string()).collect();
                let raw = exprs.join(&format!(",\n{}    ", new_indent));
                format!("\n{}    {}", new_indent, raw)
            })
            .unwrap_or_else(|| format!("\n{}    erlang:error(not_implemented)", new_indent));

        if is_first {
            if use_snippet {
                format_to!(fn_def, "\n\n{}$0{}({})", new_indent, name, args_str);
            } else {
                format_to!(fn_def, "\n\n{}{}({})", new_indent, name, args_str);
            }
        } else {
            format_to!(fn_def, "\n{}{}({})", new_indent, name, args_str);
        }

        if let Some(guard) = guard_text {
            fn_def.push_str(&guard);
        }

        fn_def.push_str(" ->");
        fn_def.push_str(&body_text);

        // Add return value if needed
        if !outliving_locals.is_empty() {
            let ret = match outliving_locals {
                [local] => local.as_string(ctx.db().upcast()),
                vars => format!(
                    "{{{}}}",
                    vars.iter()
                        .map(|v| v.as_string(ctx.db().upcast()))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            };
            format_to!(fn_def, ",\n{}    {}", new_indent, ret);
        }

        if is_last {
            fn_def.push('.');
        } else {
            fn_def.push(';');
        }
    }

    fn_def
}

/// Compute variables that are bound inside the case expression and used afterward
fn compute_outliving_locals(
    ctx: &AssistContext<'_>,
    case_syntax: &SyntaxNode,
    locals_bound_in_body: &FxHashSet<Resolution>,
) -> Vec<Var> {
    // Find the parent clause body
    let parent = case_syntax.ancestors().find_map(ast::ClauseBody::cast);
    let parent = match parent {
        Some(p) => p,
        None => return vec![],
    };

    // Find expressions after the case expression
    let case_range = case_syntax.text_range();
    let trailing: Vec<ast::Expr> = parent
        .exprs()
        .filter(|expr| expr.syntax().text_range().start() > case_range.end())
        .collect();

    if trailing.is_empty() {
        return vec![];
    }

    let mut analyzer = ScopeAnalysis::new();
    for expr in trailing {
        analyzer.walk_ast_expr(&ctx.sema, ctx.file_id(), expr);
    }

    locals_bound_in_body
        .iter()
        .filter_map(|local| {
            // Check if the variable is used (free) or pattern-matched (bound)
            // in trailing expressions. In Erlang, a "bound" variable in trailing
            // code means it's being asserted/matched against, not rebound —
            // so both cases indicate the variable is "used" after the case.
            if analyzer.free.contains(local) || analyzer.bound.contains(local) {
                Some(local.0)
            } else {
                None
            }
        })
        .collect()
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn basic_case_no_free_vars() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         {ok, V} -> V;
         error -> default
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X).

                $0fun_name_edited({ok, V}) ->
                    V;
                fun_name_edited(error) ->
                    default.
            "#]],
        )
    }

    #[test]
    fn case_with_free_vars() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X, Default) ->
     ca~se X of
         {ok, V} -> V;
         error -> Default
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X, Default) ->
                    fun_name_edited(X, Default).

                $0fun_name_edited({ok, V}, Default) ->
                    V;
                fun_name_edited(error, Default) ->
                    Default.
            "#]],
        )
    }

    #[test]
    fn case_with_guards() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         V when is_integer(V) -> V + 1;
         _ -> 0
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X).

                $0fun_name_edited(V) when is_integer(V) ->
                    V + 1;
                fun_name_edited(_) ->
                    0.
            "#]],
        )
    }

    #[test]
    fn case_single_clause() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         {ok, V} -> V
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X).

                $0fun_name_edited({ok, V}) ->
                    V.
            "#]],
        )
    }

    #[test]
    fn not_applicable_outside_case() {
        check_assist_not_applicable(
            case_to_function_clauses,
            r#"
 -module(main).
 fo~o(X) -> X.
"#,
        )
    }

    #[test]
    fn case_with_multi_expression_body() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         {ok, V} ->
             log(V),
             V;
         error ->
             log(error),
             default
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X).

                $0fun_name_edited({ok, V}) ->
                    log(V),
                    V;
                fun_name_edited(error) ->
                    log(error),
                    default.
            "#]],
        )
    }

    #[test]
    fn case_with_complex_scrutinee() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se lists:reverse(X) of
         [H | _] -> H;
         [] -> empty
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(lists:reverse(X)).

                $0fun_name_edited([H | _]) ->
                    H;
                fun_name_edited([]) ->
                    empty.
            "#]],
        )
    }

    #[test]
    fn not_applicable_with_macro_clause() {
        check_assist_not_applicable(
            case_to_function_clauses,
            r#"
 -module(main).
 -define(CLAUSE, ok -> ok).
 foo(X) ->
     ca~se X of
         ?CLAUSE;
         error -> default
     end.
"#,
        )
    }

    #[test]
    fn case_with_outliving_locals() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     Y = ca~se X of
         {ok, V} -> V;
         error -> default
     end,
     {ok, Y}.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    Y = fun_name_edited(X),
                    {ok, Y}.

                $0fun_name_edited({ok, V}) ->
                    V;
                fun_name_edited(error) ->
                    default.
            "#]],
        )
    }

    #[test]
    fn name_collision_avoidance() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 fun_name() -> existing.
 foo(X) ->
     ca~se X of
         ok -> 1;
         error -> 0
     end.
"#,
            expect![[r#"
                -module(main).
                fun_name() -> existing.
                foo(X) ->
                    fun_name1_edited(X).

                $0fun_name1_edited(ok) ->
                    1;
                fun_name1_edited(error) ->
                    0.
            "#]],
        )
    }

    #[test]
    fn nested_case_extracts_inner() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     case X of
         {ok, Y} ->
             ca~se Y of
                 1 -> one;
                 _ -> other
             end;
         error -> default
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    case X of
                        {ok, Y} ->
                            fun_name_edited(Y);
                        error -> default
                    end.

                $0fun_name_edited(1) ->
                    one;
                fun_name_edited(_) ->
                    other.
            "#]],
        )
    }

    #[test]
    fn case_as_function_argument() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     bar(ca~se X of
         ok -> 1;
         error -> 0
     end).
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    bar(fun_name_edited(X)).

                $0fun_name_edited(ok) ->
                    1;
                fun_name_edited(error) ->
                    0.
            "#]],
        )
    }

    #[test]
    fn multiple_free_vars() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X, A, B) ->
     ca~se X of
         ok -> A + B;
         error -> A - B
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X, A, B) ->
                    fun_name_edited(X, A, B).

                $0fun_name_edited(ok, A, B) ->
                    A + B;
                fun_name_edited(error, A, B) ->
                    A - B.
            "#]],
        )
    }

    #[test]
    fn tuple_pattern() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         {A, B, C} -> A + B + C;
         _ -> 0
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X).

                $0fun_name_edited({A, B, C}) ->
                    A + B + C;
                fun_name_edited(_) ->
                    0.
            "#]],
        )
    }

    #[test]
    fn wildcard_only_clause() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         _ -> ok
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X).

                $0fun_name_edited(_) ->
                    ok.
            "#]],
        )
    }

    #[test]
    fn binary_pattern() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         <<H, Rest/binary>> -> {H, Rest};
         <<>> -> empty
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X).

                $0fun_name_edited(<<H, Rest/binary>>) ->
                    {H, Rest};
                fun_name_edited(<<>>) ->
                    empty.
            "#]],
        )
    }

    #[test]
    fn case_not_last_expression() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     R = ca~se X of
         ok -> 1;
         error -> 0
     end,
     log(R),
     {result, R}.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    R = fun_name_edited(X),
                    log(R),
                    {result, R}.

                $0fun_name_edited(ok) ->
                    1;
                fun_name_edited(error) ->
                    0.
            "#]],
        )
    }

    // Regression test: when the scrutinee variable X is also used
    // in a clause body, X must still be available in the extracted
    // function. Since X is the scrutinee (first argument) but clause
    // patterns may not bind it, X must also be passed as a free var.
    #[test]
    fn scrutinee_var_used_in_body() {
        check_assist(
            case_to_function_clauses,
            "Extract case to function clauses",
            r#"
 -module(main).
 foo(X) ->
     ca~se X of
         1 -> X + 10;
         _ -> X
     end.
"#,
            expect![[r#"
                -module(main).
                foo(X) ->
                    fun_name_edited(X, X).

                $0fun_name_edited(1, X) ->
                    X + 10;
                fun_name_edited(_, X) ->
                    X.
            "#]],
        )
    }
}
