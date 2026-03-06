/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: list_head_reverse_to_last
//!
//! warn on code of the form `hd(lists:reverse(L))` and suggest `lists:last(L)`

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::Category;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct InefficientLastLinter;

impl Linter for InefficientLastLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnnecessaryReversalToFindLastElementOfList
    }

    fn description(&self) -> &'static str {
        "Unnecessary intermediate reverse list allocated."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Warning
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PatternKind {
    /// `hd(lists:reverse(List))`
    Hd,
    /// `lists:nth(1, lists:reverse(List))`
    Nth,
    /// `[LastElem|_] = lists:reverse(List)`
    Pat,
}

impl SsrPatternsLinter for InefficientLastLinter {
    type Context = PatternKind;

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, PatternKind)> = vec![
                (
                    format!("ssr: hd(lists:reverse({LIST_VAR}))."),
                    PatternKind::Hd,
                ),
                (
                    format!("ssr: lists:nth(1, lists:reverse({LIST_VAR}))."),
                    PatternKind::Nth,
                ),
                (
                    format!("ssr: [{LAST_ELEM_VAR}|_] = lists:reverse({LIST_VAR})."),
                    PatternKind::Pat,
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
            PatternKind::Hd | PatternKind::Nth => make_fix_hd(sema, file_id, matched),
            PatternKind::Pat => make_fix_pat(sema, file_id, matched),
        }
    }

    fn add_categories(&self, context: &Self::Context) -> Vec<Category> {
        match context {
            PatternKind::Pat => vec![Category::SimplificationRule],
            _ => vec![],
        }
    }
}

pub(crate) static LINTER: InefficientLastLinter = InefficientLastLinter;

static LIST_VAR: &str = "_@List";
static LAST_ELEM_VAR: &str = "_@LastElem";

fn make_fix_hd(sema: &Semantic, file_id: FileId, matched: &Match) -> Option<Vec<Assist>> {
    let inefficient_call_range = matched.range.range;
    let list_arg = matched.placeholder_text(sema, LIST_VAR)?;
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_last = format!("lists:last({list_arg})");
    builder.replace(inefficient_call_range, efficient_last);
    Some(vec![fix(
        "list_head_reverse_to_last",
        "Rewrite to use lists:last/1",
        builder.finish(),
        inefficient_call_range,
    )])
}

fn make_fix_pat(sema: &Semantic, file_id: FileId, matched: &Match) -> Option<Vec<Assist>> {
    let inefficient_call_range = matched.range.range;
    let list_arg = matched.placeholder_text(sema, LIST_VAR)?;
    let last_elem_binding = matched.placeholder_text(sema, LAST_ELEM_VAR)?;
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_last = format!("{last_elem_binding} = lists:last({list_arg})");
    builder.replace(inefficient_call_range, efficient_last);
    Some(vec![fix(
        "unnecessary_reversal_to_find_last_element_of_list",
        "Rewrite to use lists:last/1",
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
        d.code == DiagnosticCode::UnnecessaryReversalToFindLastElementOfList
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
    fn detects_inefficient_last_via_hd() {
        check_diagnostics(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> hd(lists:reverse(List)).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0029: Unnecessary intermediate reverse list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_last_via_hd() {
        check_fix(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> hd(lists:re~verse(List)).
            "#,
            expect![[r#"
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:last(List).
            "#]],
        )
    }

    #[test]
    fn detects_inefficient_last_via_pattern() {
        check_diagnostics(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> [LastElem|_] = lists:reverse(List), LastElem.
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0029: Unnecessary intermediate reverse list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_last_via_pattern() {
        check_fix(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> [LastElem|_] = lists:re~verse(List), LastElem.
            "#,
            expect![[r#"
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> LastElem = lists:last(List), LastElem.
            "#]],
        )
    }

    #[test]
    fn ignores_inefficient_last_via_nth_when_index_is_not_one() {
        check_diagnostics(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:nth(3, lists:reverse(List)).
            "#,
        )
    }

    #[test]
    fn detects_inefficient_last_via_nth() {
        check_diagnostics(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:nth(1, lists:reverse(List)).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0029: Unnecessary intermediate reverse list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_last_via_nth() {
        check_fix(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:nth(1, lists:re~verse(List)).
            "#,
            expect![[r#"
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:last(List).
            "#]],
        )
    }
}
