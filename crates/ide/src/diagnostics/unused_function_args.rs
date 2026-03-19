/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint/fix: unused_function_args
//!
//! Return a diagnostic if a function has an argument that is not used in
//! the function body, and offer to add an underscore to the name as fix.

use std::collections::HashMap;
use std::collections::HashSet;

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use elp_syntax::ast;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::FunctionClauseDef;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use super::Category;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

pub(crate) struct UnusedFunctionArgsLinter;

impl Linter for UnusedFunctionArgsLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedFunctionArg
    }

    fn description(&self) -> &'static str {
        "this variable is unused"
    }

    fn is_experimental(&self) -> bool {
        // TODO: Remove experimental once T151727890 is resolved
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Context {
    new_name: String,
}

impl GenericLinter for UnusedFunctionArgsLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let mut res = Vec::new();
        sema.def_map(file_id)
            .get_function_clauses()
            .for_each(|(_, def)| {
                if def.file.file_id != file_id {
                    return;
                }
                process_clause(&mut res, sema, def);
            });
        Some(res)
    }

    fn tag(&self, _context: &Context) -> Option<super::DiagnosticTag> {
        Some(super::DiagnosticTag::Unused)
    }

    fn fixes(
        &self,
        context: &Context,
        range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let mut edit_builder = TextEdit::builder();
        edit_builder.replace(range, context.new_name.clone());
        let edit = edit_builder.finish();
        Some(vec![fix(
            "prefix_with_underscore",
            "Prefix variable with an underscore",
            SourceChange::from_text_edit(file_id, edit),
            range,
        )])
    }

    fn add_categories(&self, _context: &Context) -> Vec<Category> {
        vec![Category::Experimental, Category::SimplificationRule]
    }
}

pub(crate) static LINTER: UnusedFunctionArgsLinter = UnusedFunctionArgsLinter;

fn process_clause(
    res: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    def: &FunctionClauseDef,
) {
    let source_file = sema.parse(def.file.file_id);

    let in_clause = def.in_clause(sema, def);
    let body_map = in_clause.get_body_map();
    let clause = in_clause.clone().body;

    let pats = &clause.clause.pats;
    let mut unused_vars_with_wrong_name = HashMap::new();

    for clause_arg_pat_id in pats.iter() {
        in_clause.fold_pat(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            *clause_arg_pat_id,
            (),
            &mut |(), ctx| {
                if let AnyExprId::Pat(pat_id) = ctx.item_id
                    && let Some(var) = match ctx.item {
                        AnyExpr::Pat(pat) => pat.as_var(),
                        _ => None,
                    }
                    && is_unused_var(sema, &in_clause, &body_map, &source_file, &pat_id)
                {
                    let var_name = var.as_string(sema.db.upcast());
                    if !var_name.starts_with('_') {
                        unused_vars_with_wrong_name.insert(pat_id, var_name);
                    }
                }
            },
        );
    }

    if !unused_vars_with_wrong_name.is_empty()
        && let Some(replacements) =
            pick_new_unused_var_names(sema, &in_clause, &unused_vars_with_wrong_name)
    {
        for (pat_id, new_name) in replacements.iter() {
            if let Some(range) = in_clause.range_for_pat(*pat_id)
                && range.file_id == def.file.file_id
            {
                res.push(GenericLinterMatchContext {
                    range: FileRange {
                        file_id: range.file_id,
                        range: range.range,
                    },
                    context: Context {
                        new_name: new_name.clone(),
                    },
                });
            }
        }
    }
}

fn is_unused_var(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionClauseDef>,
    body_map: &hir::BodySourceMap,
    source_file: &InFile<ast::SourceFile>,
    pat_id: &PatId,
) -> bool {
    match &in_clause[*pat_id].as_var() {
        Some(_var) => {
            if let Some(infile_ast_ptr) = body_map.pat(*pat_id)
                && let Some(ast::Expr::ExprMax(ast::ExprMax::Var(ast_var))) =
                    infile_ast_ptr.to_node(source_file)
            {
                let infile_ast_var = InFile::new(source_file.file_id, &ast_var);
                if let Some(var_usages) = sema.find_local_usages_ast(infile_ast_var) {
                    return var_usages.len() == 1;
                }
            }
            false
        }
        _ => false,
    }
}

fn pick_new_unused_var_names(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionClauseDef>,
    unused_var_names: &HashMap<PatId, String>,
) -> Option<HashMap<PatId, String>> {
    let clause_vars = hir::ScopeAnalysis::clause_vars_in_scope(sema, &in_clause.with_value(()))?;
    let unused_vars: HashSet<hir::Var> = HashSet::from_iter(
        unused_var_names
            .keys()
            .filter_map(|pat_id| in_clause[*pat_id].as_var()),
    );
    let other_ignored_var_names: HashSet<String> =
        HashSet::from_iter(clause_vars.iter().filter_map(|v| {
            if unused_vars.contains(v) {
                None
            } else {
                let vname = v.as_string(sema.db.upcast());
                if !vname.starts_with('_') {
                    None
                } else {
                    Some(vname)
                }
            }
        }));
    let result = unused_var_names
        .iter()
        .map(|(k, v)| {
            let mut new_var_name = format!("_{v}");
            while other_ignored_var_names.contains(&new_var_name) {
                new_var_name = format!("_{new_var_name}");
            }
            (*k, new_var_name)
        })
        .collect();
    Some(result)
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn check_diagnostic_unused_unprefixed_variables() {
        check_diagnostics(
            r#"
            -module(main).
            do_something(Unused, Used, _AlsoUsed, AlsoUnused, _UnusedButOk) ->
                     %%% ^^^^^^ 💡 warning: W0010: this variable is unused
                     %%%                          ^^^^^^^^^^ 💡 warning: W0010: this variable is unused

                case Used of
                  undefined -> ok;
                  _Unused -> do_something_else(_AlsoUsed)
                end.
            "#,
        );
    }

    #[test]
    fn check_diagnostic_unused_unprefixed_variables_inside_patterns() {
        check_diagnostics(
            r#"
            -module(main).
            do_something([Unused | Used], #{foo := {_AlsoUsed, AlsoUnused, _UnusedButOk}}) ->
                     %%%  ^^^^^^ 💡 warning: W0010: this variable is unused
                     %%%                                       ^^^^^^^^^^ 💡 warning: W0010: this variable is unused

                case Used of
                  undefined -> ok;
                  _Unused -> do_something_else(_AlsoUsed)
                end.
            "#,
        );
    }

    #[test]
    fn check_prefixes_unused_unprefixed_variables() {
        check_fix(
            r#"
                -module(main).
                do_something(U~nused, Used, _AlsoUsed, _UnusedButOk) ->
                    case Used of
                      undefined -> ok;
                      _SomethingElseUnused -> do_something_else(_AlsoUsed)
                    end.
                "#,
            expect![[r#"
                -module(main).
                do_something(_Unused, Used, _AlsoUsed, _UnusedButOk) ->
                    case Used of
                      undefined -> ok;
                      _SomethingElseUnused -> do_something_else(_AlsoUsed)
                    end.
                "#]],
        );
    }

    #[test]
    fn check_prefixes_unused_unprefixed_variables_avoiding_captures() {
        check_fix(
            r#"
                -module(main).
                do_something(Un~used, Used, _AlsoUsed, _UnusedButOk) ->
                    case Used of
                      undefined -> ok;
                      _Unused -> do_something_else(_AlsoUsed)
                    end.
                "#,
            expect![[r#"
                -module(main).
                do_something(__Unused, Used, _AlsoUsed, _UnusedButOk) ->
                    case Used of
                      undefined -> ok;
                      _Unused -> do_something_else(_AlsoUsed)
                    end.
                "#]],
        );
    }

    #[test]
    fn argument_used_in_macro_1() {
        check_diagnostics(
            r#"
               -module(main).
               -define(a_macro(Expr), ok).
               %% -define(a_macro(Expr), {Expr}).
               get_aclink_state_test_helper(Args) ->
                   ?a_macro(Args).
                "#,
        );
    }

    #[test]
    fn more_than_one_clause() {
        check_diagnostics(
            r#"
               -module(main).
               foo(Args) -> {foo, Args};
               foo(Args2) -> ok.
               %%  ^^^^^ 💡 warning: W0010: this variable is unused
                "#,
        );
    }

    #[test]
    fn usage_only_in_pattern_is_ok() {
        check_diagnostics(
            r#"
               -module(main).
               foo(X, _Y = [_Z = {X, _, _} | _]) -> bar;
               foo(X, _Y) -> pub.
               %%  ^ 💡 warning: W0010: this variable is unused
            "#,
        );
    }
}
