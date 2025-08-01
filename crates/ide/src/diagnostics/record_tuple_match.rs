/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: record-tuple-match
//
// Return a warning if a record is unpacked as a bare tuple, rather than using record syntax.

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_text_edit::TextRange;
use hir::AnyExpr;
use hir::FunctionDef;
use hir::Literal;
use hir::Name;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
use crate::Diagnostic;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _file_kind| {
        record_tuple_match(diags, sema, file_id);
    },
};

fn record_tuple_match(acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.for_each_function(file_id, |def| check_function(acc, sema, def));
}

fn check_function(acc: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    let def_fb = def.in_function_body(sema, def);
    def_fb.clone().fold_function(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
        },
        (),
        &mut |_acc, clause_id, ctx| {
            let in_clause = def_fb.in_clause(clause_id);
            if let AnyExpr::Pat(Pat::Tuple { pats }) = &ctx.item {
                if let Some(pat) = pats.first() {
                    if let Pat::Literal(Literal::Atom(atom)) = &in_clause[*pat] {
                        let def_map = sema.def_map(def.file.file_id);
                        let record_name = &(*atom).as_name(sema.db.upcast());
                        if let Some(record) = def_map.get_record(record_name) {
                            if pats.len() == record.record.fields.len() + 1 {
                                let range = in_clause.range_for_pat(*pat);
                                report(def.file.file_id, record_name, range, acc);
                            }
                        }
                    }
                }
            };
        },
    )
}

fn report(
    file_id: FileId,
    record_name: &Name,
    range: Option<FileRange>,
    acc: &mut Vec<Diagnostic>,
) -> Option<()> {
    let range = range?;
    if range.file_id == file_id {
        acc.push(make_diagnostic(range.range, record_name));
    }

    None
}

fn make_diagnostic(range: TextRange, record_name: &Name) -> Diagnostic {
    let message = format!("matching record '{record_name}' as a tuple.");
    Diagnostic::new(DiagnosticCode::RecordTupleMatch, message, range)
        .with_severity(Severity::Warning)
}

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;

    #[test]
    fn match_bare_tuple_arity_match() {
        check_diagnostics(
            r#"
             -module(main).

             -record(my_rec, {field1, field2}).
             tt(X) ->
                 A = #my_rec{field1 = 4, field2 = 2},
                 {my_rec, Field1, _} = X,
              %%  ^^^^^^ warning: matching record 'my_rec' as a tuple.
                 {A, Field1}.
            "#,
        );
    }

    #[test]
    fn match_bare_tuple_arity_mismatch() {
        check_diagnostics(
            r#"
             -module(main).

             -record(my_rec, {field1, field2}).
             tt(X) ->
                 A = #my_rec{field1 = 4, field2 = 2},
                 {my_rec, Field1} = X,
                 {A, Field1}.
            "#,
        );
    }

    #[test]
    fn match_bare_tuple_function_args() {
        check_diagnostics(
            r#"
             -module(main).

             -record(my_rec, {field1, field2}).
             tt({my_rec, Field1, _}) ->
              %% ^^^^^^ warning: matching record 'my_rec' as a tuple.
                 A = #my_rec{field1 = 4, field2 = 2},
                 {A, Field1}.
            "#,
        );
    }
}
