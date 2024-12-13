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
//! Return a diagnostic if a function has an argument that is not used in
//! the function body, and offer to add an underscore to the name as fix.

use std::collections::HashMap;
use std::collections::HashSet;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::ast;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::FunctionClauseDef;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use text_edit::TextEdit;
use text_edit::TextRange;

use super::Category;
use super::Diagnostic;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
use crate::diagnostics::DiagnosticCode;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        // TODO: disable this check when T151727890 and T151605845 are resolved
        experimental: true,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        unused_function_args(diags, sema, file_id);
    },
};

fn unused_function_args(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.def_map(file_id)
        .get_function_clauses()
        .for_each(|(_, def)| {
            if def.file.file_id != file_id {
                return;
            }
            let source_file = sema.parse(file_id);

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
                    &mut |(), ctx| match ctx.item_id {
                        AnyExprId::Pat(pat_id) => {
                            if let Some(var) = match ctx.item {
                                AnyExpr::Pat(pat) => pat.as_var(),
                                _ => None,
                            } {
                                if is_unused_var(sema, &in_clause, &body_map, &source_file, &pat_id)
                                {
                                    let var_name = var.as_string(sema.db.upcast());
                                    if !var_name.starts_with('_') {
                                        unused_vars_with_wrong_name.insert(pat_id, var_name);
                                    }
                                }
                            }
                        }
                        _ => {}
                    },
                );
            }

            if !unused_vars_with_wrong_name.is_empty() {
                if let Some(replacements) =
                    pick_new_unused_var_names(sema, &in_clause, &unused_vars_with_wrong_name)
                {
                    for (pat_id, new_name) in replacements.iter() {
                        if let Some(range) = in_clause.range_for_pat(*pat_id) {
                            diags.push(make_diagnostic(file_id, range, new_name.clone()));
                        }
                    }
                }
            }
        });
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
            if let Some(infile_ast_ptr) = body_map.pat(*pat_id) {
                if let Some(ast::Expr::ExprMax(ast::ExprMax::Var(ast_var))) =
                    infile_ast_ptr.to_node(source_file)
                {
                    let infile_ast_var = InFile::new(source_file.file_id, &ast_var);
                    if let Some(var_usages) = sema.find_local_usages_ast(infile_ast_var) {
                        return var_usages.len() == 1;
                    }
                }
            }
            false
        }
        _ => false,
    }
}

// Pick a new name for the unused vars, that start with an underscore. Ensure no clash with existing names.
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
            let mut new_var_name = format!("_{}", v);
            while other_ignored_var_names.contains(&new_var_name) {
                new_var_name = format!("_{}", new_var_name);
            }
            (*k, new_var_name)
        })
        .collect();
    Some(result)
}

fn make_diagnostic(file_id: FileId, range: TextRange, new_name: String) -> Diagnostic {
    let mut edit_builder = TextEdit::builder();
    edit_builder.replace(range, new_name);
    let edit = edit_builder.finish();

    Diagnostic::new(
        DiagnosticCode::UnusedFunctionArg,
        "this variable is unused",
        range,
    )
    .with_severity(Severity::Warning)
    .unused()
    .add_categories([
        // Marking as EXPERIMENTAL since it currently conflicts with handlers::ignore_variable https://fburl.com/code/rkm52yfj
        Category::Experimental,
        Category::SimplificationRule,
    ])
    .with_fixes(Some(vec![fix(
        "prefix_with_underscore",
        "Prefix variable with an underscore",
        SourceChange::from_text_edit(file_id, edit),
        range,
    )]))
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
                     %%% ^^^^^^ 💡 warning: this variable is unused
                     %%%                          ^^^^^^^^^^ 💡 warning: this variable is unused

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
                     %%%  ^^^^^^ 💡 warning: this variable is unused
                     %%%                                       ^^^^^^^^^^ 💡 warning: this variable is unused

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
               %%  ^^^^^ 💡 warning: this variable is unused
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
               %%  ^ 💡 warning: this variable is unused
            "#,
        );
    }
}
