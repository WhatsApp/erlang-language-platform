/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint/fix: redundant_assignment
//!
//! Return a diagnostic whenever we have A = B, with A unbound, and offer to inline
//! A as a fix.

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextRange;
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
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::codemod_helpers::check_is_only_place_where_var_is_defined_ast;
use crate::codemod_helpers::check_var_has_references;
use crate::diagnostics::Category;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) struct RedundantAssignmentLinter;

impl Linter for RedundantAssignmentLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::RedundantAssignment
    }

    fn description(&self) -> &'static str {
        "assignment is redundant"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }

    fn is_experimental(&self) -> bool {
        // TODO: disable this check when T151727890 and T151605845 are resolved
        true
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Context {
    renamings: SourceChange,
}

impl GenericLinter for RedundantAssignmentLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        if sema.db.generated_status(file_id).is_generated() {
            return Some(vec![]);
        }
        let mut res = Vec::new();
        sema.for_each_function(file_id, |def| process_matches(&mut res, sema, def));
        Some(res)
    }

    fn fixes(
        &self,
        context: &Context,
        range: TextRange,
        _sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<Assist>> {
        Some(vec![fix(
            "remove_redundant_assignment",
            "Use right-hand of assignment everywhere",
            context.renamings.clone(),
            range,
        )])
    }

    fn add_categories(&self, _context: &Context) -> Vec<Category> {
        vec![Category::SimplificationRule]
    }
}

pub(crate) static LINTER: RedundantAssignmentLinter = RedundantAssignmentLinter;

fn process_matches(
    res: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    def: &FunctionDef,
) {
    let def_fb = def.in_function_body(sema, def);
    def_fb.clone().fold_function(
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        (),
        &mut |_acc, clause_id, ctx| {
            let in_clause = def_fb.in_clause(clause_id);
            if let AnyExprId::Expr(expr_id) = ctx.item_id
                && let AnyExpr::Expr(Expr::Match { lhs, rhs }) = ctx.item
                && let Pat::Var(_) = &in_clause[lhs]
                && let Expr::Var(_) = &in_clause[rhs]
                && let Some(match_ctx) = is_var_assignment_to_unused_var(
                    sema,
                    in_clause,
                    def.file.file_id,
                    expr_id,
                    lhs,
                    rhs,
                )
            {
                res.push(match_ctx);
            }
        },
    );
}

fn is_var_assignment_to_unused_var(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    expr_id: ExprId,
    lhs: PatId,
    rhs: ExprId,
) -> Option<GenericLinterMatchContext<Context>> {
    let source_file = sema.parse(file_id);
    let body_map = in_clause.get_body_map();

    let rhs_name = body_map.expr(rhs)?.to_node(&source_file)?.to_string();

    let renamings = try_rename_usages(sema, &body_map, &source_file, lhs, rhs_name)?;

    let range = in_clause.range_for_expr(expr_id)?;
    if range.file_id == file_id {
        Some(GenericLinterMatchContext {
            range: FileRange {
                file_id: range.file_id,
                range: range.range,
            },
            context: Context { renamings },
        })
    } else {
        None
    }
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

        check_is_only_place_where_var_is_defined_ast(sema, infile_ast_var)?;
        check_var_has_references(sema, infile_ast_var)?; // otherwise covered by trivial-match

        if let hir::DefinitionOrReference::Definition(var_def) = def {
            let sym_def = elp_ide_db::SymbolDefinition::Var(var_def);
            return sym_def
                .rename(
                    sema,
                    &new_name,
                    &|_| false,
                    elp_ide_db::rename::SafetyChecks::No,
                )
                .ok();
        }
    }
    None
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

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
            expect![[r#"
            -module(main).

            do_foo() ->
              X = 42,
              X = X,
              bar(X),
              X.
            "#]],
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
            %%  ^^^^^ 💡 weak: W0009: assignment is redundant
                bar(Y),
                Z = Y,
            %%  ^^^^^ 💡 weak: W0009: assignment is redundant
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
