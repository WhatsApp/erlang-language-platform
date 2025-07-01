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
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_text_edit::TextRange;
use hir::AnyExpr;
use hir::AnyExprRef;
use hir::Expr;
use hir::ExprSource;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::ParentId;
use hir::fold::fold_file_functions;

use super::Diagnostic;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: true,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _file_kind| {
        check_file(diags, sema, &file_id);
    },
};

/// Look for macro expansions. If we find one, check what the top-level expansion gives us.
fn check_file(acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: &FileId) {
    let fold_strategy = Strategy {
        macros: MacroStrategy::ExpandButIncludeMacroCall,
        parens: ParenStrategy::VisibleParens,
    };
    let index_strategy = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::VisibleParens,
    };

    fold_file_functions(sema, fold_strategy, *file_id, (), &mut |_acc, ctx| {
        if let AnyExpr::Expr(Expr::MacroCall { expansion, .. }) = &ctx.item {
            if let Some((body, _body_map, ast)) = ctx.body_with_expr_source(sema) {
                let visible_parens_body = body.index_with_strategy(index_strategy);
                if let Expr::BinaryOp { .. } = &visible_parens_body[*expansion] {
                    if let ParentId::HirIdx(hir_idx) = ctx.parent() {
                        if hir_idx.body_origin == ctx.body_origin {
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
                                        make_diagnostic(acc, file_id, ast);
                                    }
                                }
                            };
                        }
                    };
                }
            }
        }
    });
}

fn make_diagnostic(acc: &mut Vec<Diagnostic>, file_id: &FileId, ast: ExprSource) {
    let range = ast.range();
    if range.file_id == *file_id {
        let fix = add_parens_fix(*file_id, &range.range);
        acc.push(
            Diagnostic::new(
                DiagnosticCode::MacroPrecedenceEscape,
                "The macro expansion can have unexpected precedence here",
                range.range,
            )
            .with_severity(Severity::Warning)
            .with_fixes(Some(vec![fix])),
        );
    }
}

fn add_parens_fix(file_id: FileId, range: &TextRange) -> Assist {
    let assist_message = "Add parens to macro call".to_string();
    let edit = add_parens_edit(range);
    fix(
        "macro_precedence_add_parens",
        &assist_message,
        SourceChange::from_text_edit(file_id, edit.clone()),
        *range,
    )
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;

    use crate::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

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
              %%               ^^^^^^^^^^ 💡 warning: The macro expansion can have unexpected precedence here
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
              %%               ^^^^^^^^^^^ 💡 warning: The macro expansion can have unexpected precedence here
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
    #[ignore = "our ELP macro lowering is not able to process this expansion"]
    fn in_guard() {
        check_diagnostics(
            r#"
             -module(main).
             -define(IS_FROBBABLE(X), is_tuple(X), element(1, X) =:= frob).
             foo(A) ->
               case A of
                  X when is_map(X) orelse ?IS~_FROBBABLE(X) -> ok
              %%                          ^^^^^^^^^^^^^^^^^^ 💡 warning: The macro expansion can have unexpected precedence here
               end.
            "#,
        );
    }
}
