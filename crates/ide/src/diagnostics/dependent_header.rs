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

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_syntax::ast::RecordName;
use hir::AnyExpr;
use hir::InFile;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use super::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct DependentHeaderLinter;

impl Linter for DependentHeaderLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::DependentHeader
    }

    fn description(&self) -> &'static str {
        "Record not defined in this context"
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id) == FileKind::Header
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Context {
    record_name: String,
}

impl GenericLinter for DependentHeaderLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let def_map = sema.def_map(file_id);
        let source_file = sema.parse(file_id);
        let form_list = sema.form_list(file_id);
        let mut res = Vec::new();
        for (define_id, _define) in form_list.define_attributes() {
            let (body, body_map) = sema
                .db
                .define_body_with_source(InFile::new(file_id, define_id));
            body.body.fold_expr(
                Strategy {
                    macros: MacroStrategy::Expand,
                    parens: ParenStrategy::InvisibleParens,
                },
                body.expr,
                (),
                &mut |acc, ctx| {
                    if let Some(name) = match ctx.item {
                        AnyExpr::Expr(expr) => expr.as_record_name().cloned(),
                        _ => None,
                    } {
                        let record_name = sema.db.lookup_atom(name);
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
                                context: Context {
                                    record_name: record_name.to_string(),
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
        Cow::Owned(format!(
            "Record '{}' not defined in this context",
            context.record_name
        ))
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
%%                ^^^^^^^^^^ 💡 warning: W0015: Record 'my_record' not defined in this context
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
%%                              ^^^^^^^^^^ 💡 warning: W0015: Record 'my_record' not defined in this context
            "#,
        )
    }

    #[test]
    fn test_dependent_header_record_index() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO, #my_record.my_field).
%%                ^^^^^^^^^^ 💡 warning: W0015: Record 'my_record' not defined in this context
            "#,
        )
    }

    #[test]
    fn test_dependent_header_record_update() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO(Record), Record#my_record{my_field = 42}).
%%                              ^^^^^^^^^^ 💡 warning: W0015: Record 'my_record' not defined in this context
            "#,
        )
    }
}
