/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: redundant_assignment
//!
//! Return a diagnostic whenever we have A = B, with A unbound, and offer to inline
//! A as a fix.
//!

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::ast;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::BodySourceMap;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::Pat;
use hir::PatId;
use hir::Semantic;

use super::Diagnostic;
use super::Severity;
use crate::codemod_helpers::check_is_only_place_where_var_is_defined;
use crate::codemod_helpers::check_var_has_references;
use crate::diagnostics::Category;
use crate::diagnostics::DiagnosticCode;
use crate::fix;

pub(crate) fn redundant_assignment(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.def_map(file_id)
        .get_functions()
        .iter()
        .for_each(|(_arity, def)| {
            if def.file.file_id == file_id {
                process_matches(diags, sema, def)
            }
        });
}

fn process_matches(diags: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    let def_fb = def.in_function_body(sema.db, def);
    def_fb
        .clone()
        .fold_function((), &mut |_acc, clause_id, ctx| {
            let in_clause = def_fb.in_clause(clause_id);
            match ctx.item_id {
                AnyExprId::Expr(expr_id) => {
                    if let AnyExpr::Expr(Expr::Match { lhs, rhs }) = ctx.item {
                        if let Pat::Var(_) = &in_clause[lhs] {
                            if let Expr::Var(_) = &in_clause[rhs] {
                                if let Some(diag) = is_var_assignment_to_unused_var(
                                    sema,
                                    &in_clause,
                                    def.file.file_id,
                                    expr_id,
                                    lhs,
                                    rhs,
                                ) {
                                    diags.push(diag);
                                }
                            }
                        }
                    }
                }

                _ => {}
            }
        });
}

fn is_var_assignment_to_unused_var(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    expr_id: ExprId,
    lhs: PatId,
    rhs: ExprId,
) -> Option<Diagnostic> {
    let source_file = sema.parse(file_id);
    let body_map = in_clause.get_body_map(sema.db);

    let rhs_name = body_map.expr(rhs)?.to_node(&source_file)?.to_string();

    let renamings = try_rename_usages(sema, &body_map, &source_file, lhs, rhs_name)?;

    let range = in_clause.range_for_expr(sema.db, expr_id)?;

    let diag = Diagnostic::new(
        DiagnosticCode::RedundantAssignment,
        "assignment is redundant",
        range,
    )
    .with_severity(Severity::WeakWarning)
    .add_categories([Category::SimplificationRule])
    .with_fixes(Some(vec![fix(
        "remove_redundant_assignment",
        "Use right-hand of assignment everywhere",
        renamings,
        range,
    )]));

    Some(diag)
}

fn try_rename_usages(
    sema: &Semantic,
    body_map: &BodySourceMap,
    source_file: &InFile<ast::SourceFile>,
    pat_id: PatId,
    new_name: String,
) -> Option<SourceChange> {
    let infile_ast_ptr = body_map.pat(pat_id)?;
    let ast_node = infile_ast_ptr.to_node(source_file)?;
    if let ast::Expr::ExprMax(ast::ExprMax::Var(ast_var)) = ast_node {
        let infile_ast_var = InFile::new(source_file.file_id, &ast_var);
        let def = sema.to_def(infile_ast_var)?;

        check_is_only_place_where_var_is_defined(sema, infile_ast_var)?;
        check_var_has_references(sema, infile_ast_var)?; // otherwise covered by trivial-match

        if let hir::DefinitionOrReference::Definition(var_def) = def {
            let sym_def = elp_ide_db::SymbolDefinition::Var(var_def);
            return sym_def
                .rename(
                    sema,
                    &|_| new_name.clone(),
                    elp_ide_db::rename::SafetyChecks::No,
                )
                .ok();
        }
    }
    None
}

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn can_fix_lhs_is_var() {
        check_fix(
            r#"
            -module(main).

            do_foo() ->
              X = 42,
              ~Y = X,
              bar(Y),
              Y.
            "#,
            r#"
            -module(main).

            do_foo() ->
              X = 42,
              X = X,
              bar(X),
              X.
            "#,
        )
    }

    #[test]
    fn produces_diagnostic_lhs_is_var() {
        check_diagnostics(
            r#"
            -module(main).

            do_foo() ->
                X = 42,
                Y = X,
            %%% ^^^^^ 💡 weak: assignment is redundant
                bar(Y),
                Z = Y,
            %%% ^^^^^ 💡 weak: assignment is redundant
                g(Z),
                case Y of
                  [A] -> C = A;
                  B -> C = B
                end,
                C.
            "#,
        )
    }
}
