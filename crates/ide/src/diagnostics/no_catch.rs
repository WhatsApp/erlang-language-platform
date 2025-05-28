/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::DiagnosticCode;
use elp_syntax::AstNode;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::BodySourceMap;
use hir::Expr;
use hir::FunctionDef;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::NoCatch;
const DIAGNOSTIC_MESSAGE: &str = "Avoid `catch`.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::Warning;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diagnostics, sema, file_id, _ext| {
        sema.for_each_function(file_id, |def| check_function(diagnostics, sema, def));
    },
};

fn check_function(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    let def_fb = def.in_function_body(sema, def);
    def_fb.fold_function(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
        },
        (),
        &mut |_acc, clause_id, ctx| {
            if let AnyExpr::Expr(Expr::Catch { expr: _ }) = ctx.item {
                let map = def_fb.get_body_map(clause_id);
                if let Some(diagnostic) = make_diagnostic(sema, &map, ctx.item_id) {
                    diagnostics.push(diagnostic);
                }
            };
        },
    )
}

fn make_diagnostic(sema: &Semantic, map: &BodySourceMap, item_id: AnyExprId) -> Option<Diagnostic> {
    match item_id {
        AnyExprId::Expr(expr_id) => {
            let ast_ptr = map.expr(expr_id)?;
            match &ast_ptr.to_ast(sema.db) {
                elp_syntax::ast::Expr::CatchExpr(catch_expr) => {
                    let catch_keyword = catch_expr.syntax().first_token()?;
                    let range = catch_keyword.text_range();
                    let diagnostic = Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, range)
                        .with_severity(DIAGNOSTIC_SEVERITY);
                    Some(diagnostic)
                }
                _ => None,
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;

    #[test]
    fn basic() {
        check_diagnostics(
            r#"
              -module(main).
              -export([catcher/2]).

              catcher(X,Y) ->
              case catch X/Y of
              %%   ^^^^^ warning: Avoid `catch`.
                {'EXIT', {badarith,_}} -> "uh oh";
                N -> N
              end.
              "#,
        )
    }
}
