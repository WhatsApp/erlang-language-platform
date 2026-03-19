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

use std::borrow::Cow;

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use hir::AnyExpr;
use hir::Literal;
use hir::Name;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct RecordTupleMatchLinter;

impl Linter for RecordTupleMatchLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::RecordTupleMatch
    }

    fn description(&self) -> &'static str {
        "Matching record as a tuple."
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Context {
    record_name: Name,
}

impl GenericLinter for RecordTupleMatchLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let mut res = Vec::new();
        sema.for_each_function(file_id, |def| {
            let def_fb = def.in_function_body(sema, def);
            def_fb.clone().fold_function(
                Strategy {
                    macros: MacroStrategy::ExpandButIncludeMacroCall,
                    parens: ParenStrategy::InvisibleParens,
                },
                (),
                &mut |_acc, clause_id, ctx| {
                    let in_clause = def_fb.in_clause(clause_id);
                    if let AnyExpr::Pat(Pat::Tuple { pats }) = &ctx.item
                        && let Some(pat) = pats.first()
                        && let Pat::Literal(Literal::Atom(atom)) = &in_clause[*pat]
                    {
                        let def_map = sema.def_map(def.file.file_id);
                        let record_name = &(*atom).as_name(sema.db.upcast());
                        if let Some(record) = def_map.get_record(record_name)
                            && pats.len() == record.record.fields.len() + 1
                        {
                            let range = in_clause.range_for_pat(*pat);
                            if let Some(range) = range
                                && range.file_id == def.file.file_id
                            {
                                res.push(GenericLinterMatchContext {
                                    range: FileRange {
                                        file_id: range.file_id,
                                        range: range.range,
                                    },
                                    context: Context {
                                        record_name: record_name.clone(),
                                    },
                                });
                            }
                        }
                    };
                },
            )
        });
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Matching record '{}' as a tuple.",
            context.record_name
        ))
    }
}

pub(crate) static LINTER: RecordTupleMatchLinter = RecordTupleMatchLinter;

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
              %%  ^^^^^^ 💡 warning: W0027: Matching record 'my_rec' as a tuple.
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
              %% ^^^^^^ 💡 warning: W0027: Matching record 'my_rec' as a tuple.
                 A = #my_rec{field1 = 4, field2 = 2},
                 {A, Field1}.
            "#,
        );
    }
}
