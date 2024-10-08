/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: boolean-precedence
//
// Return a warning for the usage of `and` or `or`. Unlike other
// languages, in Erlang these have high precedence. See
// https://www.erlang.org/doc/system/expressions.html#operator-precedence
// So this often results in incorrect or buggy code.

use std::fmt;
use std::fmt::Display;

use elp_ide_assists::helpers::expr_needs_parens;
use elp_ide_assists::helpers::include_preceding_whitespace;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::DiagnosticCode;
use elp_syntax::ast;
use elp_syntax::ast::BinaryOp;
use elp_syntax::ast::LogicOp;
use elp_syntax::AstNode;
use hir::fold::AnyCallBackCtx;
use hir::fold::MacroStrategy;
use hir::AnyExpr;
use hir::ClauseId;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionBody;
use hir::Semantic;
use hir::Strategy;
use text_edit::TextEdit;
use text_edit::TextRange;

use super::Diagnostic;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        boolean_precedence(diags, sema, file_id);
    },
};

fn boolean_precedence(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.for_each_function(file_id, |def| check_function(diagnostics, sema, def));
}

fn check_function(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    let def_fb = def.in_function_body(sema, def);
    def_fb.clone().fold_function(
        Strategy {
            macros: MacroStrategy::VisibleMacros,
        },
        (),
        &mut |_acc, clause_id, ctx| {
            let op = match &ctx.item {
                AnyExpr::Expr(Expr::BinaryOp { lhs, rhs, op }) => match op {
                    BinaryOp::LogicOp(LogicOp::And { lazy: false }) => {
                        if ctx.in_guard() {
                            Some((Op::AndInGuard, *lhs, *rhs))
                        } else {
                            Some((Op::And, *lhs, *rhs))
                        }
                    }
                    BinaryOp::LogicOp(LogicOp::Or { lazy: false }) => Some((Op::Or, *lhs, *rhs)),
                    _ => None,
                },
                _ => None,
            };
            if let Some(op) = op {
                report(
                    sema,
                    &def_fb,
                    def.file.file_id,
                    clause_id,
                    ctx,
                    op,
                    diagnostics,
                );
            }
        },
    )
}

fn is_complex(
    sema: &Semantic,
    def_fb: &InFunctionBody<&FunctionDef>,
    file_id: FileId,
    clause_id: ClauseId,
    expr: ExprId,
) -> Option<(ast::Expr, bool)> {
    // Note: lowering to HIR strips parens.  We may have to check the
    // parent node for them, when deciding to add or not.
    let map = def_fb.get_body_map(clause_id);
    let expr_source = map.expr(expr)?;
    let source = sema.db.parse(file_id).tree();
    if let Some(expr) = expr_source.to_node(&InFile::new(file_id, source)) {
        Some((expr.clone(), expr_needs_parens(&expr)))
    } else {
        None
    }
}

fn report(
    sema: &Semantic,
    def_fb: &InFunctionBody<&FunctionDef>,
    file_id: FileId,
    clause_id: ClauseId,
    ctx: AnyCallBackCtx,
    binop: (Op, ExprId, ExprId),
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<()> {
    let (op, lhs, rhs) = binop;
    let (lhs_expr, lhs_complex) = is_complex(sema, def_fb, file_id, clause_id, lhs)?;
    let (rhs_expr, rhs_complex) = is_complex(sema, def_fb, file_id, clause_id, rhs)?;
    if !lhs_complex && !rhs_complex {
        return None;
    }
    let map = def_fb.get_body_map(clause_id);
    let expr_source = map.any(ctx.item_id)?;
    let source = sema.db.parse(file_id).tree();
    if let Some(ast::Expr::BinaryOpExpr(binop)) = expr_source.to_node(&InFile::new(file_id, source))
    {
        let (_op, token) = binop.op()?;
        let range = token.text_range();
        let preceding_ws_range = include_preceding_whitespace(&token);
        let mut d = make_diagnostic(file_id, range, preceding_ws_range, op)
            .with_ignore_fix(sema, def_fb.file_id());
        if lhs_complex {
            add_parens_fix(file_id, &range, &lhs_expr, "LHS", &mut d);
        }
        if rhs_complex {
            add_parens_fix(file_id, &range, &rhs_expr, "RHS", &mut d);
        }
        diagnostics.push(d)
    }
    Some(())
}

fn add_parens_fix(
    file_id: FileId,
    range: &TextRange,
    expr: &ast::Expr,
    where_str: &str,
    diag: &mut Diagnostic,
) {
    // We have a complex expression. Add parens, if the parent does not already have them.
    if let Some(parent) = expr.syntax().parent() {
        if let Some(ast::Expr::ExprMax(ast::ExprMax::ParenExpr(_))) = ast::Expr::cast(parent) {
            // Already has parens, do nothing.
        } else {
            let expr_range = expr.syntax().text_range();
            let assist_message = format!("Add parens to {}", where_str);
            let mut builder = TextEdit::builder();
            builder.insert(expr_range.start(), "(".to_string());
            builder.insert(expr_range.end(), ")".to_string());
            let edit = builder.finish();
            diag.add_fix(fix(
                "replace_boolean_operator_add_parens",
                &assist_message,
                SourceChange::from_text_edit(file_id, edit.clone()),
                range.clone(),
            ));
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    And,
    AndInGuard,
    Or,
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::And => write!(f, "and"),
            Op::AndInGuard => write!(f, "and"),
            Op::Or => write!(f, "or"),
        }
    }
}

impl Op {
    fn preferred(&self) -> &str {
        match self {
            Op::And => "andalso",
            Op::AndInGuard => ",",
            Op::Or => "orelse",
        }
    }

    /// Return either the bare token range, or the one including
    /// preceding whitespace, depending on what the preferred replacement is.
    fn range(&self, range: TextRange, preceding_ws_range: TextRange) -> TextRange {
        match self {
            Op::AndInGuard => preceding_ws_range,
            _ => range,
        }
    }
}

fn make_diagnostic(
    file_id: FileId,
    range: TextRange,
    preceding_ws_range: TextRange,
    op: Op,
) -> Diagnostic {
    let message = format!(
        "Consider using the short-circuit expression '{}' instead of '{}'.\nOr add parentheses to avoid potential ambiguity.",
        op.preferred(),
        op,
    );
    let assist_message = format!("Replace '{}' with '{}'", op, op.preferred());
    let edit = TextEdit::replace(
        op.range(range, preceding_ws_range),
        op.preferred().to_string(),
    );
    Diagnostic::new(DiagnosticCode::BooleanPrecedence, message, range)
        .with_severity(Severity::Warning)
        .with_fixes(Some(vec![fix(
            "replace_boolean_operator",
            &assist_message,
            SourceChange::from_text_edit(file_id, edit.clone()),
            range,
        )]))
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_specific_fix;

    #[test]
    fn avoid_and() {
        check_diagnostics(
            r#"
            -module(main).
            foo() ->
              F = fun(X) ->
                my_is_integer(X) and X > 0 end,
          %%                     ^^^ 💡 warning: Consider using the short-circuit expression 'andalso' instead of 'and'.
          %%                       | Or add parentheses to avoid potential ambiguity.
              F.

            my_is_integer(_X) -> true.
                      "#,
        )
    }

    #[test]
    fn avoid_and_ok_with_simple_types() {
        check_diagnostics(
            r#"
            -module(main).
            foo() ->
              F = fun(X) -> my_is_bool(X) and X end,
              F.

            my_is_bool(_X) -> true.
                      "#,
        )
    }

    #[test]
    fn avoid_or() {
        check_diagnostics(
            r#"
            -module(main).
            foo(X) ->
              predicate(X) or X > 10.
          %%               ^^ 💡 warning: Consider using the short-circuit expression 'orelse' instead of 'or'.
          %%                | Or add parentheses to avoid potential ambiguity.

            predicate(_X) -> false.
                      "#,
        )
    }

    #[test]
    fn ignore_avoid_and() {
        check_specific_fix(
            "Ignore problem",
            r#"
                -module(main).
                foo() ->
                  F = fun(X) ->
                    my_is_integer(X) a~nd X > 0 end,
                %%                   ^^^ 💡 warning: Consider using the short-circuit expression 'andalso' instead of 'and'.
                %%                     | Or add parentheses to avoid potential ambiguity.
                  F.

                my_is_integer(_X) -> true.
            "#,
            expect![[r#"
                  -module(main).
                  foo() ->
                    F = fun(X) ->
                      % elp:ignore W0025 (boolean_precedence)
                      my_is_integer(X) and X > 0 end,
                    F.

                  my_is_integer(_X) -> true.
            "#]],
        )
    }

    #[test]
    fn replace_and() {
        check_specific_fix(
            "Replace 'and' with 'andalso'",
            r#"
            -module(main).
            foo() ->
              F = fun(X) ->
                my_is_integer(X) a~nd X > 0 end,
            %%                   ^^^ 💡 warning: Consider using the short-circuit expression 'andalso' instead of 'and'.
            %%                     | Or add parentheses to avoid potential ambiguity.
              F.

            my_is_integer(_X) -> true."#,
            expect![[r#"
                  -module(main).
                  foo() ->
                    F = fun(X) ->
                      my_is_integer(X) andalso X > 0 end,
                    F.

                  my_is_integer(_X) -> true."#]],
        )
    }

    #[test]
    fn replace_and_in_guard() {
        check_specific_fix(
            "Replace 'and' with ','",
            r#"
            -module(main).
            foo(X) when X < 10 a~nd X > 0 ->
               X + 1.
            %%                 ^^^ 💡 warning: Consider using the short-circuit expression ',' instead of 'and'.
            %%                   | Or add parentheses to avoid potential ambiguity.
            "#,
            expect![[r#"
                -module(main).
                foo(X) when X < 10, X > 0 ->
                   X + 1.
            "#]],
        )
    }

    #[test]
    fn add_parens_and_rhs() {
        check_specific_fix(
            "Add parens to RHS",
            r#"
            -module(main).
            foo() ->
              F = fun(X) ->
                my_is_integer(X) a~nd X > 0 end,
            %%                   ^^^ 💡 warning: Consider using the short-circuit expression 'andalso' instead of 'and'.
            %%                     | Or add parentheses to avoid potential ambiguity.
              F.

            my_is_integer(_X) -> true."#,
            expect![[r#"
                  -module(main).
                  foo() ->
                    F = fun(X) ->
                      my_is_integer(X) and (X > 0) end,
                    F.

                  my_is_integer(_X) -> true."#]],
        )
    }

    #[test]
    fn add_parens_or_lhs() {
        check_specific_fix(
            "Add parens to LHS",
            r#"
            -module(main).
            foo() ->
              F = fun(X) ->
                X < 0 o~r X > 10 end,
            %%                   ^^^ 💡 warning: Consider using the short-circuit expression 'orelse' instead of 'or'.
            %%                     | Or add parentheses to avoid potential ambiguity.
              F.

            my_is_integer(_X) -> true."#,
            expect![[r#"
                  -module(main).
                  foo() ->
                    F = fun(X) ->
                      (X < 0) or X > 10 end,
                    F.

                  my_is_integer(_X) -> true."#]],
        )
    }

    #[test]
    fn replace_or() {
        check_specific_fix(
            "Replace 'or' with 'orelse'",
            r#"
            -module(main).
            foo(X) ->
              predicate(X) o~r X > 10.
            %%             ^^ 💡 warning: Consider using the short-circuit expression 'orelse' instead of 'or'.
            %%              | Or add parentheses to avoid potential ambiguity.

            predicate(_X) -> false."#,
            expect![[r#"
                  -module(main).
                  foo(X) ->
                    predicate(X) orelse X > 10.

                  predicate(_X) -> false."#]],
        )
    }
}
