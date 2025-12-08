/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: boolean-precedence
//
// Return a warning for the usage of `and` or `or`. Unlike other
// languages, in Erlang these have high precedence. See
// https://www.erlang.org/doc/system/expressions.html#operator-precedence
// So this often results in incorrect or buggy code.

use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;

use elp_ide_assists::helpers::add_parens_edit;
use elp_ide_assists::helpers::include_preceding_whitespace;
use elp_ide_assists::helpers::unwrap_parens;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use elp_syntax::ast;
use elp_syntax::ast::BinaryOp;
use elp_syntax::ast::LogicOp;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::AnyExprRef;
use hir::ClauseId;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionBody;
use hir::Semantic;
use hir::Strategy;
use hir::fold::AnyCallBackCtx;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::ParentId;

use super::GenericLinter;
use super::GenericLinterMatchContext;
use super::Linter;
use crate::Assist;
use crate::fix;

pub(crate) struct BooleanPrecedenceLinter;

impl Linter for BooleanPrecedenceLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::BooleanPrecedence
    }

    fn description(&self) -> &'static str {
        "boolean precedence"
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    range: TextRange,
    preceding_ws_range: TextRange,
    op: Op,
    lhs_complex: bool,
    rhs_complex: bool,
    add_parens_range: TextRange,
}

impl GenericLinter for BooleanPrecedenceLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let mut res = Vec::new();
        sema.for_each_function(file_id, |def| {
            check_function(&mut res, sema, def);
        });
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        format!(
            "Consider using the short-circuit expression '{}' instead of '{}'.\nOr add parentheses to avoid potential ambiguity.",
            context.op.preferred(),
            context.op,
        )
        .into()
    }

    fn fixes(
        &self,
        context: &Self::Context,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let mut fixes = Vec::new();

        // Add "replace with preferred operator" fix
        let assist_message = format!("Replace '{}' with '{}'", context.op, context.op.preferred());
        let edit = TextEdit::replace(
            context.op.range(context.range, context.preceding_ws_range),
            context.op.preferred().to_string(),
        );
        fixes.push(fix(
            "replace_boolean_operator",
            &assist_message,
            SourceChange::from_text_edit(file_id, edit),
            context.range,
        ));

        // Add "add parens" fixes if applicable
        if context.lhs_complex {
            fixes.push(parens_fix("LHS", file_id, context));
        }
        if context.rhs_complex {
            fixes.push(parens_fix("RHS", file_id, context));
        }

        Some(fixes)
    }
}

fn parens_fix(side: &str, file_id: FileId, context: &Context) -> Assist {
    let assist_message = format!("Add parens to {side}");
    let edit = add_parens_edit(&context.add_parens_range);
    fix(
        "replace_boolean_operator_add_parens",
        &assist_message,
        SourceChange::from_text_edit(file_id, edit),
        context.range,
    )
}

fn check_function(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    def: &FunctionDef,
) {
    let def_fb = def.in_function_body(sema, def);
    def_fb.clone().fold_function(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
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
                collect_match(matches, sema, &def_fb, def.file.file_id, clause_id, ctx, op);
            }
        },
    )
}

fn collect_match(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    def_fb: &InFunctionBody<&FunctionDef>,
    file_id: FileId,
    clause_id: ClauseId,
    ctx: AnyCallBackCtx,
    binop: (Op, ExprId, ExprId),
) -> Option<()> {
    /*
    we have (inserting parens for tree)
      (A and B) > C
    We need to turn it into
      A and (B > C)
    OR
      A andalso B > C
    */
    let (binop, b_lhs, b_rhs) = binop;
    let body = def_fb.body(clause_id);
    if let ParentId::HirIdx(hir_idx) = ctx.parent()
        && let AnyExprRef::Expr(Expr::BinaryOp { lhs, rhs, op: _ }) = &body.get_any(hir_idx.idx)
    {
        let lhs_complex = AnyExprId::Expr(*rhs) == ctx.item_id;
        let rhs_complex = AnyExprId::Expr(*lhs) == ctx.item_id;
        if !lhs_complex && !rhs_complex {
            return None;
        }
        let map = def_fb.get_body_map(clause_id);
        let add_parens_range = if rhs_complex {
            let b_rhs_ast_ptr = map.expr(b_rhs)?;
            let rhs_ast_ptr = map.expr(*rhs)?;
            TextRange::new(
                b_rhs_ast_ptr.range().range.start(),
                rhs_ast_ptr.range().range.end(),
            )
        } else {
            let lhs_ast_ptr = map.expr(*lhs)?;
            let b_lhs_ast_ptr = map.expr(b_lhs)?;
            TextRange::new(
                lhs_ast_ptr.range().range.start(),
                b_lhs_ast_ptr.range().range.end(),
            )
        };
        let expr_source = map.any(ctx.item_id)?;
        let source = sema.db.parse(file_id).tree();
        if let Some(ast::Expr::BinaryOpExpr(binop_ast)) =
            unwrap_parens(&expr_source.to_node(&InFile::new(file_id, source))?)
        {
            let (_op, token) = binop_ast.op()?;
            let range = token.text_range();
            let preceding_ws_range = include_preceding_whitespace(&token);

            matches.push(GenericLinterMatchContext {
                range,
                context: Context {
                    range,
                    preceding_ws_range,
                    op: binop,
                    lhs_complex,
                    rhs_complex,
                    add_parens_range,
                },
            });
        }
    };
    Some(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Op {
    #[default]
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

pub static LINTER: BooleanPrecedenceLinter = BooleanPrecedenceLinter;

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_specific_fix;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::MacroPrecedenceEscape);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn avoid_and() {
        check_diagnostics(
            r#"
            -module(main).
            foo() ->
              F = fun(X) ->
                my_is_integer(X) and X > 0 end,
          %%                     ^^^ 💡 warning: W0025: Consider using the short-circuit expression 'andalso' instead of 'and'.
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
    fn or_ok_for_safe_macro_call() {
        check_diagnostics(
            r#"
            -module(main).
            -define(MM(X), (X > 3)).
            foo(S,P) ->
                S or ?MM(P).
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
          %%               ^^ 💡 warning: W0025: Consider using the short-circuit expression 'orelse' instead of 'or'.
          %%                | Or add parentheses to avoid potential ambiguity.

            predicate(_X) -> false.
                      "#,
        )
    }

    #[test]
    fn avoid_or_in_parened_expr() {
        check_diagnostics(
            r#"
            -module(main).
            foo(S,P) ->
                (S or P > 3).
          %%       ^^ 💡 warning: W0025: Consider using the short-circuit expression 'orelse' instead of 'or'.
          %%        | Or add parentheses to avoid potential ambiguity.
                      "#,
        )
    }

    #[test]
    fn avoid_or_in_parened_expr_2() {
        check_diagnostics(
            r#"
            -module(main).
            foo(S,P) ->
                ((S or P > 3)).
          %%        ^^ 💡 warning: W0025: Consider using the short-circuit expression 'orelse' instead of 'or'.
          %%         | Or add parentheses to avoid potential ambiguity.
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
                %%                   ^^^ 💡 warning: W0025: Consider using the short-circuit expression 'andalso' instead of 'and'.
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
            %%                   ^^^ 💡 warning: W0025: Consider using the short-circuit expression 'andalso' instead of 'and'.
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
            %%                 ^^^ 💡 warning: W0025: Consider using the short-circuit expression ',' instead of 'and'.
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
            %%                   ^^^ 💡 warning: W0025: Consider using the short-circuit expression 'andalso' instead of 'and'.
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
            %%        ^^ 💡 warning: W0025: Consider using the short-circuit expression 'orelse' instead of 'or'.
            %%         | Or add parentheses to avoid potential ambiguity.
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
            %%             ^^ 💡 warning: W0025: Consider using the short-circuit expression 'orelse' instead of 'or'.
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
