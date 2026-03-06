/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: lists_zip_lists_seq_to_lists_enumerate
//!
//! warn on code of the form `lists:zip(lists:seq(1,length(L)),L)` and suggest `lists:enumerate(L)`

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::SubId;
use hir::AnyExprId;
use hir::BasedInteger;
use hir::Expr;
use hir::Literal;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::Category;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct InefficientEnumerateLinter;

impl Linter for InefficientEnumerateLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::ListsZipWithSeqRatherThanEnumerate
    }

    fn description(&self) -> &'static str {
        "Unnecessary intermediate list allocated."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PatternKind {
    /// `lists:zip(lists:seq(Index, length(List)), List)`
    TwoArg,
    /// `lists:zip(lists:seq(Index, Step, length(List)), List)`
    ThreeArg,
}

impl SsrPatternsLinter for InefficientEnumerateLinter {
    type Context = PatternKind;

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, PatternKind)> = vec![
                (
                    format!(
                        "ssr: lists:zip(lists:seq({INDEX_VAR},length({LIST_VAR})),{LIST_VAR})."
                    ),
                    PatternKind::TwoArg,
                ),
                (
                    format!(
                        "ssr: lists:zip(lists:seq({INDEX_VAR},{STEP_VAR},length({LIST_VAR})),{LIST_VAR})."
                    ),
                    PatternKind::ThreeArg,
                ),
            ];
        }
        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<bool> {
        if let Some(comments) = matched.comments(sema)
            && !comments.is_empty()
        {
            return None;
        }
        Some(true)
    }

    fn fixes(
        &self,
        context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        match context {
            PatternKind::TwoArg => {
                if is_indexing_from_literal_one(sema, matched) {
                    make_fix_simple(sema, file_id, matched)
                } else {
                    make_fix_custom_index(sema, file_id, matched)
                }
            }
            PatternKind::ThreeArg => make_fix_custom_index_and_step(sema, file_id, matched),
        }
    }

    fn add_categories(&self, context: &Self::Context) -> Vec<Category> {
        match context {
            PatternKind::ThreeArg => vec![Category::SimplificationRule],
            _ => vec![],
        }
    }
}

pub(crate) static LINTER: InefficientEnumerateLinter = InefficientEnumerateLinter;

static LIST_VAR: &str = "_@List";
static INDEX_VAR: &str = "_@Index";
static STEP_VAR: &str = "_@Step";

fn is_indexing_from_literal_one(sema: &Semantic, m: &Match) -> bool {
    || -> Option<bool> {
        let index_match = m.get_placeholder_match(sema, INDEX_VAR)?;
        let body = &m.matched_node_body.get_body(sema)?;
        match index_match.code_id {
            SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match body[expr_id] {
                Expr::Literal(Literal::Integer(BasedInteger { base: _, value: 1 })) => Some(true),
                _ => Some(false),
            },
            _ => Some(false),
        }
    }()
    .unwrap_or(false)
}

fn make_fix_simple(sema: &Semantic, file_id: FileId, matched: &Match) -> Option<Vec<Assist>> {
    let inefficient_call_range = matched.range.range;
    let list_arg_matches = matched.placeholder_texts(sema, LIST_VAR)?;
    let list_arg = list_arg_matches.first()?;
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_enumerate = format!("lists:enumerate({list_arg})");
    builder.replace(inefficient_call_range, efficient_enumerate);
    Some(vec![fix(
        "lists_zip_lists_seq_to_lists_enumerate",
        "Rewrite to use lists:enumerate/1",
        builder.finish(),
        inefficient_call_range,
    )])
}

fn make_fix_custom_index(sema: &Semantic, file_id: FileId, matched: &Match) -> Option<Vec<Assist>> {
    let inefficient_call_range = matched.range.range;
    let list_arg_matches = matched.placeholder_texts(sema, LIST_VAR)?;
    let list_arg = list_arg_matches.first()?;
    let index_arg = matched.placeholder_text(sema, INDEX_VAR)?;
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_enumerate = format!("lists:enumerate({index_arg}, {list_arg})");
    builder.replace(inefficient_call_range, efficient_enumerate);
    Some(vec![fix(
        "lists_zip_lists_seq_to_lists_enumerate",
        "Rewrite to use lists:enumerate/2",
        builder.finish(),
        inefficient_call_range,
    )])
}

fn make_fix_custom_index_and_step(
    sema: &Semantic,
    file_id: FileId,
    matched: &Match,
) -> Option<Vec<Assist>> {
    let inefficient_call_range = matched.range.range;
    let list_arg_matches = matched.placeholder_texts(sema, LIST_VAR)?;
    let list_arg = list_arg_matches.first()?;
    let index_arg = matched.placeholder_text(sema, INDEX_VAR)?;
    let step_arg = matched.placeholder_text(sema, STEP_VAR)?;
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_enumerate = format!("lists:enumerate({index_arg}, {step_arg}, {list_arg})");
    builder.replace(inefficient_call_range, efficient_enumerate);
    Some(vec![fix(
        "list_zip_with_seq_rather_than_enumerate",
        "Rewrite to use lists:enumerate/3",
        builder.finish(),
        inefficient_call_range,
    )])
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::ListsZipWithSeqRatherThanEnumerate
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    #[test]
    fn detects_inefficient_enumerate() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(List) -> lists:zip(lists:seq(1,length(List)),List).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0033: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn ignores_when_list_params_do_not_match() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(ListX,ListY) -> lists:zip(lists:seq(1,length(ListX)),ListY).
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_enumerate_over_length_call() {
        check_fix(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:zip(lists:s~eq(1, length(List)), List).
            "#,
            expect![[r#"
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:enumerate(List).
            "#]],
        )
    }

    #[test]
    fn detects_inefficient_enumerate_custom_index() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(N, List) -> lists:zip(lists:seq(N,length(List)),List).
         %%             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0033: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_enumerate_over_length_call_custom_index() {
        check_fix(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, List) -> lists:zip(lists:s~eq(N, length(List)), List).
            "#,
            expect![[r#"
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, List) -> lists:enumerate(N, List).
            "#]],
        )
    }

    #[test]
    fn detects_inefficient_enumerate_custom_index_and_step() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(N, Step, List) -> lists:zip(lists:seq(N,Step,length(List)),List).
         %%                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0033: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_enumerate_over_length_call_custom_index_and_step() {
        check_fix(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, Step, List) -> lists:zip(lists:s~eq(N, Step, length(List)), List).
            "#,
            expect![[r#"
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, Step, List) -> lists:enumerate(N, Step, List).
            "#]],
        )
    }
}
