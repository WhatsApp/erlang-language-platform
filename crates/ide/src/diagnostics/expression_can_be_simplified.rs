/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: unused_function_args
//!
//! Return a diagnostic if an expression can be trivially simplified. Covers things like
//! `[] ++ Xs`, `0 + X`, etc. This is typically useful as a simplification rule in codemods.

use elp_ide_db::source_change::SourceChangeBuilder;
use hir::AnyExprId;
use hir::ClauseId;
use hir::FunctionDef;
use hir::InFunctionBody;
use hir::Strategy;

use crate::ast::ArithOp;
use crate::ast::BinaryOp;
use crate::ast::ListOp;
use crate::diagnostics::Category;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;
use crate::fix;
use crate::Diagnostic;
use crate::FileId;
use crate::Semantic;

pub(crate) fn diagnostic(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    if sema.db.is_generated(file_id) {
        // No point asking for changes to generated files
        return;
    }

    sema.def_map(file_id)
        .get_functions()
        .for_each(|(_, fun_def)| {
            if fun_def.file.file_id != file_id {
                return;
            }

            let def_fb = fun_def.in_function_body(sema.db, fun_def);

            def_fb.fold_function(
                Strategy::SurfaceOnly,
                Some(()),
                &mut |acc, clause_id, ctx| {
                    let simplification = match ctx.item {
                        hir::AnyExpr::Expr(hir::Expr::BinaryOp { lhs, rhs, op }) => {
                            simplify_binary_op(op, lhs, rhs, clause_id, sema, &def_fb)
                        }
                        _ => None,
                    };
                    match simplification {
                        None => {}
                        Some(replacement_str) => {
                            let expr_id = as_expr_id(ctx.item_id)?;
                            let range = def_fb.range_for_expr(sema.db, clause_id, expr_id)?;

                            let mut changes = SourceChangeBuilder::new(file_id);
                            changes.replace(range, &replacement_str);
                            let replacement = changes.finish();

                            let diag = Diagnostic::new(
                                DiagnosticCode::ExpressionCanBeSimplified,
                                format!("Can be simplified to `{}`.", &replacement_str).to_string(),
                                range,
                            )
                            .with_severity(Severity::Warning)
                            .add_categories([Category::SimplificationRule])
                            .with_fixes(Some(vec![fix(
                                "simplify_expression",
                                format!("Replace by `{}`", &replacement_str).as_str(),
                                replacement,
                                range,
                            )]));
                            diags.push(diag);
                        }
                    }
                    acc
                },
            );
        });
}

fn simplify_binary_op(
    op: BinaryOp,
    lhs_id: hir::ExprId,
    rhs_id: hir::ExprId,
    clause_id: ClauseId,
    sema: &Semantic,
    def_fb: &InFunctionBody<&FunctionDef>,
) -> Option<String> {
    let body = def_fb.body(clause_id);
    if body.is_macro(AnyExprId::Expr(lhs_id)) || body.is_macro(rhs_id.into()) {
        return None;
    }
    match (&body[lhs_id], op, &body[rhs_id]) {
        // ==== LIST OPS ====
        // ++
        (lhs, BinaryOp::ListOp(ListOp::Append), _rhs) if is_empty_list_expr(lhs) => {
            to_string(&rhs_id, sema, clause_id, def_fb)
        }
        (_lhs, BinaryOp::ListOp(ListOp::Append), rhs) if is_empty_list_expr(rhs) => {
            to_string(&lhs_id, sema, clause_id, def_fb)
        }

        // --
        (lhs, BinaryOp::ListOp(ListOp::Subtract), _rhs) if is_empty_list_expr(lhs) => {
            to_string(&lhs_id, sema, clause_id, def_fb)
        }
        (_lhs, BinaryOp::ListOp(ListOp::Subtract), rhs) if is_empty_list_expr(rhs) => {
            to_string(&lhs_id, sema, clause_id, def_fb)
        }

        // ==== ARITH OPS ====
        // +
        (lhs, BinaryOp::ArithOp(ArithOp::Add), _rhs) if is_integer(0, lhs) => {
            to_string(&rhs_id, sema, clause_id, def_fb)
        }
        (_lhs, BinaryOp::ArithOp(ArithOp::Add), rhs) if is_integer(0, rhs) => {
            to_string(&lhs_id, sema, clause_id, def_fb)
        }

        // -
        (lhs, BinaryOp::ArithOp(ArithOp::Sub), _rhs) if is_integer(0, lhs) => {
            let rhs_str = to_string(&rhs_id, sema, clause_id, def_fb)?;
            Some(format!("-{}", rhs_str))
        }
        (_lhs, BinaryOp::ArithOp(ArithOp::Sub), rhs) if is_integer(0, rhs) => {
            to_string(&lhs_id, sema, clause_id, def_fb)
        }

        // *
        // NB. Not including 0 * X = 0, etc, since the value should be 0.0 if X is a float, etc
        (lhs, BinaryOp::ArithOp(ArithOp::Mul), _rhs) if is_integer(1, lhs) => {
            to_string(&rhs_id, sema, clause_id, def_fb)
        }
        (_lhs, BinaryOp::ArithOp(ArithOp::Mul), rhs) if is_integer(1, rhs) => {
            to_string(&lhs_id, sema, clause_id, def_fb)
        }

        // div / rem
        (_lhs, BinaryOp::ArithOp(ArithOp::Div), rhs) if is_integer(1, rhs) => {
            to_string(&lhs_id, sema, clause_id, def_fb)
        }
        (_lhs, BinaryOp::ArithOp(ArithOp::Rem), rhs) if is_integer(1, rhs) => Some("0".to_string()),
        _ => None,
    }
}

fn is_empty_list_expr(expr: &hir::Expr) -> bool {
    match expr {
        hir::Expr::List { exprs, tail } => exprs.is_empty() && tail.is_none(),
        _ => false,
    }
}

fn is_integer(n: i128, expr: &hir::Expr) -> bool {
    match expr {
        hir::Expr::Literal(hir::Literal::Integer(i)) => *i == n,
        _ => false,
    }
}

fn to_string(
    expr_id: &hir::ExprId,
    sema: &Semantic,
    clause_id: ClauseId,
    def_fb: &InFunctionBody<&FunctionDef>,
) -> Option<String> {
    let body_map = def_fb.get_body_map(sema.db, clause_id);
    let source_file = sema.parse(def_fb.file_id());
    let str = body_map.expr(*expr_id)?.to_node(&source_file)?.to_string();
    Some(str)
}

fn as_expr_id(any_expre_id: hir::AnyExprId) -> Option<hir::ExprId> {
    match any_expre_id {
        hir::AnyExprId::Expr(expr_id) => Some(expr_id),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_generates_diagnostics() {
        check_diagnostics(
            r#"
  -module(main).
  list_ops(X) ->
    f([] ++ [1]),
   %% ^^^^^^^^^ 💡 warning: Can be simplified to `[1]`.
    f([2] ++ [1]),
    f(X ++ [1]),
    ok.

  arith_ops(X) ->
    f(0 + 42),
   %% ^^^^^^ 💡 warning: Can be simplified to `42`.
    f(40 + 2),
    f(X + 42),
    ok.

  f(X) -> X.
            "#,
        )
    }

    #[test]
    fn test_fixes_list_ops() {
        check_fix("f(X) -> []~ ++ X.", "f(X) -> X.");
        check_fix("f(X) -> X ++ ~[].", "f(X) -> X.");

        check_fix("f(X) -> []~ -- X.", "f(X) -> [].");
        check_fix("f(X) -> X -- ~[].", "f(X) -> X.");
    }

    #[test]
    fn test_fixes_arith_ops() {
        check_fix("f(X) -> 0~ + X.", "f(X) -> X.");
        check_fix("f(X) -> X + ~0.", "f(X) -> X.");

        check_fix("f(X) -> 0~ - X.", "f(X) -> -X.");
        check_fix("f(X) -> X - ~0.", "f(X) -> X.");

        check_fix("f(X) -> 1~ * X.", "f(X) -> X.");
        check_fix("f(X) -> X * ~1.", "f(X) -> X.");

        check_fix("f(X) -> X div ~1.", "f(X) -> X.");
        check_fix("f(X) -> X rem ~1.", "f(X) -> 0.");
    }

    #[test]
    fn not_in_macro() {
        check_diagnostics(
            r#"
            -module(main).
            -define(SOME_CONST, 1).

            foo() -> ?SOME_CONST * 1024.
            "#,
        )
    }
}
