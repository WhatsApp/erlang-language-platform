/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: dependent-header
//
// Return a warning if a header file is not self-contained.

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_syntax::ast;
use elp_syntax::ast::RecordName;
use elp_syntax::AstNode;
use hir::AnyExpr;
use hir::InFile;
use hir::Name;
use hir::Semantic;
use hir::Strategy;
use text_edit::TextRange;

use super::Diagnostic;
use super::DiagnosticCode;

pub(crate) fn dependent_header(
    diagnostics: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    file_kind: FileKind,
) -> Option<()> {
    if FileKind::Header == file_kind {
        let def_map = sema.def_map(file_id);
        let source_file = sema.parse(file_id);
        let form_list = sema.form_list(file_id);
        for (define_id, _define) in form_list.define_attributes() {
            if let Some((body, body_map)) = sema
                .db
                .define_body_with_source(InFile::new(file_id, define_id))
            {
                let form_id = form_list.find_define_form(&define_id)?;

                body.body.fold_expr(
                    Strategy::TopDown,
                    form_id,
                    body.expr,
                    (),
                    &mut |acc, ctx| {
                        if let Some(name) = match ctx.item {
                            AnyExpr::Expr(expr) => expr.as_record_name().cloned(),
                            _ => None,
                        } {
                            let record_name = sema.db.lookup_atom(name);
                            if def_map.get_record(&record_name).is_none() {
                                if let Some(in_file_ast_ptr) = body_map.any(ctx.item_id) {
                                    if let Some(expr_ast) = in_file_ast_ptr.to_node(&source_file) {
                                        let diagnostic_range = match extract_record_name(&expr_ast)
                                        {
                                            Some(name) => name.syntax().text_range(),
                                            None => expr_ast.syntax().text_range(),
                                        };
                                        let d = make_diagnostic(diagnostic_range, record_name);
                                        diagnostics.push(d);
                                    }
                                }
                            }
                        };
                        acc
                    },
                );
            };
        }
    }
    Some(())
}

fn extract_record_name(expr_ast: &ast::Expr) -> Option<RecordName> {
    match expr_ast {
        elp_syntax::ast::Expr::RecordExpr(expr) => expr.name(),
        elp_syntax::ast::Expr::RecordFieldExpr(expr) => expr.name(),
        elp_syntax::ast::Expr::RecordIndexExpr(expr) => expr.name(),
        elp_syntax::ast::Expr::RecordUpdateExpr(expr) => expr.name(),
        _ => None,
    }
}

fn make_diagnostic(range: TextRange, record_name: Name) -> Diagnostic {
    Diagnostic::warning(
        DiagnosticCode::DependentHeader,
        range,
        format!("Record '{record_name}' not defined in this context"),
    )
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
%%                ^^^^^^^^^^ warning: Record 'my_record' not defined in this context
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
%%                              ^^^^^^^^^^ warning: Record 'my_record' not defined in this context
            "#,
        )
    }

    #[test]
    fn test_dependent_header_record_index() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO, #my_record.my_field).
%%                ^^^^^^^^^^ warning: Record 'my_record' not defined in this context
            "#,
        )
    }

    #[test]
    fn test_dependent_header_record_update() {
        check_diagnostics(
            r#"
//- /include/main.hrl
-define(MY_MACRO(Record), Record#my_record{my_field = 42}).
%%                              ^^^^^^^^^^ warning: Record 'my_record' not defined in this context
            "#,
        )
    }
}
