/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: dependent-header
//
// Return a warning if a header file is not self-contained.

use std::borrow::Cow;
use std::iter;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use elp_syntax::ast;
use elp_syntax::ast::RecordName;
use fxhash::FxHashSet;
use hir::AnyExpr;
use hir::AsName;
use hir::BuiltInMacro;
use hir::InFile;
use hir::Name;
use hir::PPCondition;
use hir::PPConditionId;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::macro_name;

use super::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct DependentHeaderLinter;

impl Linter for DependentHeaderLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::DependentHeader
    }

    fn description(&self) -> &'static str {
        "Element not defined in this context"
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id) == FileKind::Header
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Context {
    UndefinedRecord { name: String },
    UndefinedMacro { name: String },
}

impl GenericLinter for DependentHeaderLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let def_map = sema.def_map(file_id);
        let source_file = sema.parse(file_id);
        let form_list = sema.form_list(file_id);
        let defined_macros = iter::once(file_id)
            .chain(def_map.get_included_files())
            .fold(FxHashSet::default(), |mut defined_macros, file_id| {
                defined_macros.extend(
                    sema.form_list(file_id)
                        .define_attributes()
                        .map(|(_, define)| define.name),
                );
                defined_macros
            });
        let mut res = Vec::new();
        for (define_id, define) in form_list.define_attributes() {
            let definition = InFile::new(file_id, define_id);
            let (body, body_map) = sema.db.define_body_with_source(definition);

            let define_ast = define.form_id.get(&source_file.value);
            let params = define_ast
                .args()
                .map(|param| param.as_name())
                .collect::<FxHashSet<_>>();
            let guarded_macros =
                condition_guaranteed_macros(&form_list, define.pp_ctx.condition, &source_file);
            for macro_call in define_ast
                .syntax()
                .descendants()
                .filter_map(ast::MacroCallExpr::cast)
            {
                if let Some(macro_name) = macro_name(&macro_call)
                    && !params.contains(macro_name.name())
                    && !BuiltInMacro::is_built_in_name(macro_name.name())
                    && !defined_macros.contains(&macro_name)
                    && !guarded_macros.contains(macro_name.name())
                    && (macro_name.arity().is_none()
                        || !defined_macros.contains(&macro_name.with_arity(None)))
                {
                    let Some(name_ast) = macro_call.name() else {
                        continue;
                    };
                    let range = TextRange::new(
                        macro_call.syntax().text_range().start(),
                        name_ast.syntax().text_range().end(),
                    );
                    res.push(GenericLinterMatchContext {
                        range: FileRange { file_id, range },
                        context: Context::UndefinedMacro {
                            name: macro_name.to_string(),
                        },
                    });
                }
            }

            body.body.fold_expr(
                Strategy {
                    macros: MacroStrategy::Expand,
                    parens: ParenStrategy::InvisibleParens,
                },
                body.expr,
                (),
                &mut |acc, ctx| {
                    if let Some(name) = match ctx.item {
                        AnyExpr::Expr(expr) => expr.as_record_name(),
                        _ => None,
                    } {
                        let record_name = name.as_name();
                        if def_map.get_record(&record_name).is_none()
                            && let Some(in_file_ast_ptr) = body_map.any(ctx.item_id)
                            && let Some(expr_ast) = in_file_ast_ptr.to_node(&source_file)
                        {
                            let range = match extract_record_name(&expr_ast) {
                                Some(name) => name.syntax().text_range(),
                                None => expr_ast.syntax().text_range(),
                            };
                            res.push(GenericLinterMatchContext {
                                range: FileRange { file_id, range },
                                context: Context::UndefinedRecord {
                                    name: record_name.to_string(),
                                },
                            });
                        }
                    };
                    acc
                },
            );
        }
        Some(res)
    }

    fn match_description(&self, context: &Context) -> Cow<'_, str> {
        let (kind, name) = match context {
            Context::UndefinedRecord { name } => ("Record", name),
            Context::UndefinedMacro { name } => ("Macro", name),
        };
        Cow::Owned(format!("{kind} '{name}' not defined in this context"))
    }
}

pub(crate) static LINTER: DependentHeaderLinter = DependentHeaderLinter;

fn extract_record_name(expr_ast: &ast::Expr) -> Option<RecordName> {
    match expr_ast {
        elp_syntax::ast::Expr::RecordExpr(expr) => expr.name(),
        elp_syntax::ast::Expr::RecordFieldExpr(expr) => expr.name(),
        elp_syntax::ast::Expr::RecordIndexExpr(expr) => expr.name(),
        elp_syntax::ast::Expr::RecordUpdateExpr(expr) => expr.name(),
        _ => None,
    }
}

fn condition_guaranteed_macros(
    form_list: &hir::FormList,
    mut cond: Option<PPConditionId>,
    source_file: &hir::InFile<ast::SourceFile>,
) -> FxHashSet<Name> {
    let mut names = FxHashSet::default();
    while let Some(cond_id) = cond {
        match &form_list[cond_id] {
            PPCondition::Ifdef { pp_ctx, name, .. } => {
                names.insert(name.clone());
                cond = pp_ctx.condition;
            }
            PPCondition::Ifndef { pp_ctx, .. } => {
                cond = pp_ctx.condition;
            }
            PPCondition::If { pp_ctx, form_id } => {
                let pp_if = form_id.get(&source_file.value);
                if let Some(cond_expr) = pp_if.cond() {
                    extract_defined_names_from_expr(&cond_expr, &mut names);
                }
                cond = pp_ctx.condition;
            }
            PPCondition::Else { prev, .. } => {
                cond = match &form_list[*prev] {
                    PPCondition::Ifndef { pp_ctx, name, .. } => {
                        names.insert(name.clone());
                        pp_ctx.condition
                    }
                    PPCondition::Ifdef { pp_ctx, .. } => pp_ctx.condition,
                    PPCondition::If { pp_ctx, .. } => pp_ctx.condition,
                    PPCondition::Elif {
                        prev: elif_prev, ..
                    } => find_chain_parent(form_list, *elif_prev),
                    PPCondition::Else { .. } | PPCondition::Endif { .. } => None,
                };
            }
            PPCondition::Elif { prev, form_id } => {
                let pp_elif = form_id.get(&source_file.value);
                if let Some(cond_expr) = pp_elif.cond() {
                    extract_defined_names_from_expr(&cond_expr, &mut names);
                }
                cond = find_chain_parent(form_list, *prev);
            }
            PPCondition::Endif { .. } => {
                cond = None;
            }
        }
    }
    names
}

fn find_chain_parent(form_list: &hir::FormList, mut prev: PPConditionId) -> Option<PPConditionId> {
    loop {
        match &form_list[prev] {
            PPCondition::Ifdef { pp_ctx, .. }
            | PPCondition::Ifndef { pp_ctx, .. }
            | PPCondition::If { pp_ctx, .. } => return pp_ctx.condition,
            PPCondition::Elif {
                prev: next_prev, ..
            } => prev = *next_prev,
            _ => return None,
        }
    }
}

fn extract_defined_names_from_expr(expr: &ast::Expr, names: &mut FxHashSet<Name>) {
    match expr {
        ast::Expr::Call(call) => {
            if let Some(ast::Expr::ExprMax(ast::ExprMax::Atom(atom))) = call.expr()
                && atom.as_name().as_str() == "defined"
            {
                if let Some(arg) = call.args().and_then(|args| args.args().next()) {
                    match arg {
                        ast::Expr::ExprMax(ast::ExprMax::Atom(arg_atom)) => {
                            names.insert(arg_atom.as_name());
                        }
                        ast::Expr::ExprMax(ast::ExprMax::Var(arg_var)) => {
                            names.insert(arg_var.as_name());
                        }
                        ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(macro_call)) => {
                            if let Some(m_name) = macro_name(&macro_call) {
                                names.insert(m_name.name().clone());
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        ast::Expr::BinaryOpExpr(bin) => {
            if let Some((ast::BinaryOp::LogicOp(ast::LogicOp::And { .. }), _)) = bin.op() {
                if let Some(lhs) = bin.lhs() {
                    extract_defined_names_from_expr(&lhs, names);
                }
                if let Some(rhs) = bin.rhs() {
                    extract_defined_names_from_expr(&rhs, names);
                }
            }
        }
        ast::Expr::ExprMax(ast::ExprMax::ParenExpr(paren)) => {
            if let Some(inner) = paren.expr() {
                extract_defined_names_from_expr(&inner, names);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;

    #[test]
    fn test_dependent_header_record() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO, #my_record{}).
%%                ^^^^^^^^^^ warning: W0015: Record 'my_record' not defined in this context
%%                         | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_dependent_header_record_not_applicable() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-record(my_record, {my_field}).
-define(MY_MACRO, #my_record{}).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_record_not_applicable_included() {
        check_diagnostics(
            r#"
//- /include/main_1.hrl
-record(my_record, {my_field}).
//- /include/main_2.hrl
-include("main_1.hrl").
-define(MY_MACRO, #my_record{}).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_record_field() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO(Record), Record#my_record.my_field).
%%                              ^^^^^^^^^^ warning: W0015: Record 'my_record' not defined in this context
%%                                       | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_dependent_header_record_index() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO, #my_record.my_field).
%%                ^^^^^^^^^^ warning: W0015: Record 'my_record' not defined in this context
%%                         | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_dependent_header_record_update() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO(Record), Record#my_record{my_field = 42}).
%%                              ^^^^^^^^^^ warning: W0015: Record 'my_record' not defined in this context
%%                                       | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_dependent_header_macro() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(NUM_PIPELINE_WORKERS(), ?ENV(num_pipeline_workers, 64)).
%%                              ^^^^ warning: W0015: Macro 'ENV/2' not defined in this context
%%                                 | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_dependent_header_macro_not_applicable() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(ENV(Key, Default), application:get_env(my_app, Key, Default)).
-define(NUM_PIPELINE_WORKERS(), ?ENV(num_pipeline_workers, 64)).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_not_applicable_included() {
        check_diagnostics(
            r#"
//- /include/main_1.hrl
-define(ENV(Key, Default), application:get_env(my_app, Key, Default)).
//- /include/main_2.hrl
-include("main_1.hrl").
-define(NUM_PIPELINE_WORKERS(), ?ENV(num_pipeline_workers, 64)).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_not_applicable_transitively_included() {
        check_diagnostics(
            r#"
//- /include/main_1.hrl
-define(ENV(Key, Default), application:get_env(my_app, Key, Default)).
//- /include/main_2.hrl
-include("main_1.hrl").
//- /include/main_3.hrl
-include("main_2.hrl").
-define(NUM_PIPELINE_WORKERS(), ?ENV(num_pipeline_workers, 64)).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_reported_once_when_expanded() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(CLEANUP(Wid), ?assertEqual(ok, Wid)).
%%                    ^^^^^^^^^^^^ warning: W0015: Macro 'assertEqual/2' not defined in this context
%%                               | 💡 <suppression>
-define(REGISTER(Wid), ?CLEANUP(Wid)).
            "#,
        )
    }

    #[test]
    fn test_dependent_header_macro_defined_with_different_arity() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(ENV(), ok).
-define(NUM_PIPELINE_WORKERS(), ?ENV(num_pipeline_workers, 64)).
%%                              ^^^^ warning: W0015: Macro 'ENV/2' not defined in this context
%%                                 | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_parameter() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(INDIRECT(Name), ?Name).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_object_like_macro_with_arguments() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(ENV, fun application:get_env/3).
-define(NUM_PIPELINE_WORKERS(), ?ENV(my_app, num_pipeline_workers, 64)).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_defined_later() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(NUM_PIPELINE_WORKERS(), ?ENV(num_pipeline_workers, 64)).
-define(ENV(Key, Default), application:get_env(my_app, Key, Default)).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_not_applicable_when_undefined_later() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(HELPER, ok).
-define(PUBLIC(), ?HELPER).
-undef(HELPER).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_redefined_before_inclusion() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(HELPER, ok).
-define(PUBLIC(), ?HELPER).
-undef(HELPER).
-define(HELPER, error).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_undefined_outer_macro() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(PUBLIC(), ?HELPER).
%%                ^^^^^^^ warning: W0015: Macro 'HELPER' not defined in this context
%%                      | 💡 <suppression>
-undef(PUBLIC).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_builtin_macro_with_arguments() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(BAD_MODULE(), ?MODULE(an_argument)).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_builtin_macro_not_applicable() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(WHERE_AM_I, {?MODULE, ?LINE}).
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_guarded_by_ifdef() {
        // Issue #297: Macro invocation guarded by ifdef should not be reported as undefined
        check_diagnostics(
            r#"
//- /include/main.hrl
-ifdef(gpb_field_record_name).
-define(gpb_field, ?gpb_field_record_name).
-else.
-define(gpb_field, undefined).
-endif.
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_guarded_by_ifdef_no_else() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-ifdef(MY_FLAG).
-define(MY_MACRO, ?MY_FLAG).
-endif.
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_in_else_of_ifdef_reported() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-ifdef(MY_FLAG).
-define(FOO, ?MY_FLAG).
-else.
-define(BAR, ?MY_FLAG).
%%           ^^^^^^^^ warning: W0015: Macro 'MY_FLAG' not defined in this context
%%                  | 💡 <suppression>
-endif.
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_in_else_of_ifndef() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-ifndef(MY_FLAG).
-define(FOO, 1).
-else.
-define(BAR, ?MY_FLAG).
-endif.
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_in_ifndef_reported() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-ifndef(MY_FLAG).
-define(FOO, ?MY_FLAG).
%%           ^^^^^^^^ warning: W0015: Macro 'MY_FLAG' not defined in this context
%%                  | 💡 <suppression>
-endif.
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_guarded_nested_ifdef() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-ifdef(OUTER).
-ifdef(INNER).
-define(COMBINED, {?OUTER, ?INNER}).
-endif.
-endif.
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_guarded_by_if_defined() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-if(defined(FEATURE_A)).
-define(RUN_FEATURE, ?FEATURE_A).
-endif.
            "#,
        );
    }

    #[test]
    fn test_dependent_header_macro_guarded_by_if_andalso_defined() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-if(defined(A) andalso defined(B)).
-define(COMBINED, {?A, ?B}).
-endif.
            "#,
        );
    }
}
