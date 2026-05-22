/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: macro-precedence
//
// Return a warning if a macro expansion exposes a top level binary
// operation that could escape the macro context.

use elp_ide_assists::Assist;
use elp_ide_assists::helpers::add_parens_edit;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use hir::AnyExpr;
use hir::AnyExprRef;
use hir::Expr;
use hir::ExprId;
use hir::FoldBody;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::ParentId;
use hir::fold::fold_file_functions;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;

#[derive(Debug, Default, Clone, PartialEq)]
pub(crate) enum MacroPrecedenceContext {
    #[default]
    MacroCallSite,
    MacroArg {
        arg_ranges: Vec<TextRange>,
    },
}

pub(crate) struct MacroPrecedenceSupriseLinter;

impl Linter for MacroPrecedenceSupriseLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MacroPrecedenceEscape
    }

    fn description(&self) -> &'static str {
        "The macro expansion can have unexpected precedence here"
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }
}

impl GenericLinter for MacroPrecedenceSupriseLinter {
    type Context = MacroPrecedenceContext;

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let fold_strategy = Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::VisibleParens,
        };
        let index_strategy = Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::VisibleParens,
        };

        let mut res = Vec::new();
        fold_file_functions(sema, fold_strategy, file_id, (), &mut |_acc, ctx| {
            if let AnyExpr::Expr(Expr::MacroCall { expansion, .. }) = &ctx.item
                && let Some((body, _body_map, ast)) = ctx.body_with_expr_source(sema)
            {
                let visible_parens_body = body.index_with_strategy(index_strategy);
                if let Expr::BinaryOp { .. } = &visible_parens_body[*expansion]
                    && let ParentId::HirIdx(hir_idx) = ctx.parent()
                    && hir_idx.body_origin == ctx.body_origin
                {
                    // We can have nested macro
                    // calls, which are not
                    // visible in the
                    // visible_parens_body. Report
                    // on the top-level one only.
                    let fold_body = body.index_with_strategy(fold_strategy);
                    match &fold_body.get_any(hir_idx.idx) {
                        AnyExprRef::Expr(Expr::MacroCall { .. }) => {}
                        _ => {
                            if let AnyExprRef::Expr(Expr::BinaryOp { .. }) =
                                &visible_parens_body.get_any(hir_idx.idx)
                            {
                                let range = ast.range();
                                res.push(GenericLinterMatchContext {
                                    range,
                                    context: MacroPrecedenceContext::MacroCallSite,
                                });
                            }
                        }
                    };
                };
            }
            // Check for macro argument match-in-binop: a BinaryOp inside
            // a macro expansion where an operand is a bare Match
            // (not wrapped in parens). This can only occur through
            // macro argument substitution, since tree-sitter
            // precedence prevents it in normal code.
            if let AnyExpr::Expr(Expr::BinaryOp { lhs, rhs, .. }) = &ctx.item
                && let Some((macro_hir_idx, _)) = ctx.in_macro
                && macro_hir_idx.body_origin == ctx.body_origin
                && let Some((body, body_map, _ast)) = ctx.body_with_expr_source(sema)
            {
                let visible_parens_body = body.index_with_strategy(index_strategy);
                let arg_ranges: Vec<TextRange> = [*lhs, *rhs]
                    .into_iter()
                    .filter(|&operand| is_bare_match_operand(&visible_parens_body, operand))
                    .filter_map(|operand| {
                        let operand_range = body_map.expr(operand)?.range();
                        (operand_range.file_id == file_id).then_some(operand_range.range)
                    })
                    .collect();
                if !arg_ranges.is_empty()
                    && let Some(macro_ast) = body_map.any(macro_hir_idx.idx)
                {
                    let range = macro_ast.range();
                    if range.file_id == file_id {
                        res.push(GenericLinterMatchContext {
                            range,
                            context: MacroPrecedenceContext::MacroArg { arg_ranges },
                        });
                    }
                }
            }
        });
        Some(res)
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let file_id = ctx.file_id;
        match context {
            MacroPrecedenceContext::MacroCallSite => {
                let edit = add_parens_edit(&range);
                let fix = fix(
                    "macro_precedence_add_parens",
                    "Add parens to macro call",
                    SourceChange::from_text_edit(file_id, edit),
                    range,
                );
                Some(vec![fix])
            }
            MacroPrecedenceContext::MacroArg { arg_ranges } => {
                let mut builder = TextEdit::builder();
                for arg_range in arg_ranges {
                    builder.insert(arg_range.start(), "(".to_string());
                    builder.insert(arg_range.end(), ")".to_string());
                }
                let fix = fix(
                    "macro_precedence_add_parens",
                    "Add parens to macro argument",
                    SourceChange::from_text_edit(file_id, builder.finish()),
                    range,
                );
                Some(vec![fix])
            }
        }
    }
}

pub static LINTER: MacroPrecedenceSupriseLinter = MacroPrecedenceSupriseLinter;

/// Check if an expression is a bare `Match` (not wrapped in parens),
/// which can only appear as a BinaryOp operand through macro argument
/// substitution since tree-sitter precedence would prevent it in
/// normal code.
fn is_bare_match_operand(body: &FoldBody, expr_id: ExprId) -> bool {
    matches!(&body[expr_id], Expr::Match { .. })
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;

    use crate::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::OldEdocSyntax)
            .disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(config, fixture);
    }

    #[test]
    fn reports_within_file() {
        check_diagnostics(
            r#"
             -module(main).
             -define(MYOP(B,C), B + C).
             foo(A,B,C) -> A * ?MYOP(B,C).
              %%               ^^^^^^^^^^ 💡 warning: W0039: The macro expansion can have unexpected precedence here
             bar(B,C) -> ?MYOP(B,C).
            "#,
        )
    }

    #[test]
    fn fine_if_parens_1() {
        check_diagnostics(
            r#"
             -module(main).
             -define(MYOP(B,C), (B + C)).
             foo(A,B,C) -> A * ?MYOP(B,C).
            "#,
        )
    }

    #[test]
    fn fine_if_parens_2() {
        check_diagnostics(
            r#"
             -module(main).
             -define(MYOP(B,C), B + C).
             foo(A,B,C) -> A * (?MYOP(B,C)).
            "#,
        )
    }

    #[test]
    fn nested_macros() {
        check_diagnostics(
            r#"
             -module(main).
             -define(MYOPA(B,C), B + C).
             -define(MYOPB(B,C), ?MYOPA(B, C)).
             -define(MYOPC(B,C), ?MYOPB(B, C)).
             foo(A,B,C) -> A * ?MYOPC(B,C).
              %%               ^^^^^^^^^^^ 💡 warning: W0039: The macro expansion can have unexpected precedence here
            "#,
        )
    }

    #[test]
    fn with_fix_1() {
        check_fix(
            r#"
             -module(main).
             -define(MYOP(B,C), B + C).
             foo(A,B,C) -> A * ?M~YOP(B,C).
              %%               ^^^^^^^^^^^ 💡 warning: The macro expansion can have unexpected precedence here
            "#,
            expect![[r#"
             -module(main).
             -define(MYOP(B,C), B + C).
             foo(A,B,C) -> A * (?MYOP(B,C)).
            "#]],
        );
    }

    #[test]
    fn match_in_macro_arg_binop() {
        check_diagnostics(
            r#"
             -module(main).
             -define(RATE(TPS, Interval), round(TPS * Interval)).
             test() -> ?RATE(_TPS = 30, 60).
              %%       ^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0039: The macro expansion can have unexpected precedence here
            "#,
        )
    }

    #[test]
    fn match_in_macro_arg_binop_with_fix() {
        check_fix(
            r#"
             -module(main).
             -define(RATE(TPS, Interval), round(TPS * Interval)).
             test() -> ?R~ATE(_TPS = 30, 60).
              %%       ^^^^^^^^^^^^^^^^^^^^ 💡 warning: The macro expansion can have unexpected precedence here
            "#,
            expect![[r#"
             -module(main).
             -define(RATE(TPS, Interval), round(TPS * Interval)).
             test() -> ?RATE((_TPS = 30), 60).
            "#]],
        );
    }

    #[test]
    fn match_in_macro_arg_binop_fine_if_parens() {
        check_diagnostics(
            r#"
             -module(main).
             -define(RATE(TPS, Interval), round(TPS * Interval)).
             test() -> ?RATE((_TPS = 30), 60).
            "#,
        )
    }

    #[test]
    fn in_guard() {
        // TODO(T256429482): This should detect the macro precedence issue in guard
        // expressions, but ELP's macro lowering is not able to process this expansion.
        // Once fixed, add the diagnostic annotation:
        //   %%                          ^^^^^^^^^^^^^^^^^^ 💡 warning: W0039: The macro expansion can have unexpected precedence here
        check_diagnostics(
            r#"
             -module(main).
             -define(IS_FROBBABLE(X), is_tuple(X), element(1, X) =:= frob).
             foo(A) ->
               case A of
                  X when is_map(X) orelse ?IS_FROBBABLE(X) -> ok
               end.
            "#,
        );
    }
}
