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
            for macro_call in define_ast
                .syntax()
                .descendants()
                .filter_map(ast::MacroCallExpr::cast)
            {
                if let Some(macro_name) = macro_name(&macro_call)
                    && !params.contains(macro_name.name())
                    && !BuiltInMacro::is_built_in_name(macro_name.name())
                    && !defined_macros.contains(&macro_name)
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
}
