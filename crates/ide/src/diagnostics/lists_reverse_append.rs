/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: lists_reverse_append
//!
//! Detect patterns of the form `lists:reverse(_@L) ++ _@T` and suggest `lists:reverse(_@L, _@T)`

use elp_ide_assists::Assist;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use hir::Semantic;

use crate::diagnostics::Linter;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct ListsReverseAppendLinter;

impl Linter for ListsReverseAppendLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::ListsReverseAppend
    }

    fn description(&self) -> &'static str {
        "Use `lists:reverse/2` instead of `lists:reverse/1 ++ Tail` for better performance."
    }
}

impl SsrPatternsLinter for ListsReverseAppendLinter {
    type Context = ();

    fn patterns(&self) -> Vec<(String, Self::Context)> {
        vec![(format!("ssr: lists:reverse({LIST_VAR}) ++ {TAIL_VAR}."), ())]
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let list_match = matched.placeholder_text(sema, LIST_VAR)?;
        let tail_match = matched.placeholder_text(sema, TAIL_VAR)?;
        let mut builder = SourceChangeBuilder::new(matched.range.file_id);
        let replacement = format!("lists:reverse({list_match}, {tail_match})");
        let range = matched.range.range;
        builder.replace(range, replacement);
        let fixes = vec![fix(
            "lists_reverse_append",
            "Use lists:reverse/2",
            builder.finish(),
            range,
        )];
        Some(fixes)
    }
}

pub(crate) static LINTER: ListsReverseAppendLinter = ListsReverseAppendLinter;

static LIST_VAR: &str = "_@L";
static TAIL_VAR: &str = "_@T";

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::ListsReverseAppend
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
    fn detects_lists_reverse_append() {
        check_diagnostics(
            r#"
        //- /src/lists_reverse_append.erl
        -module(lists_reverse_append).

        reverse_and_append(List, Tail) ->
            lists:reverse(List) ++ Tail.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0056: Use `lists:reverse/2` instead of `lists:reverse/1 ++ Tail` for better performance.
           "#,
        )
    }

    #[test]
    fn detects_lists_reverse_append_with_variables() {
        check_diagnostics(
            r#"
        //- /src/lists_reverse_append.erl
        -module(lists_reverse_append).

        process(Items, Acc) ->
            lists:reverse(Items) ++ Acc.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0056: Use `lists:reverse/2` instead of `lists:reverse/1 ++ Tail` for better performance.
           "#,
        )
    }

    #[test]
    fn ignores_regular_reverse() {
        check_diagnostics(
            r#"
        //- /src/lists_reverse_append.erl
        -module(lists_reverse_append).

        reverse_only(List) ->
            lists:reverse(List).
           "#,
        )
    }

    #[test]
    fn ignores_regular_append() {
        check_diagnostics(
            r#"
        //- /src/lists_reverse_append.erl
        -module(lists_reverse_append).

        append_only(A, B) ->
            A ++ B.
           "#,
        )
    }

    #[test]
    fn fixes_lists_reverse_append() {
        check_fix(
            r#"
        //- /src/lists_reverse_append.erl
        -module(lists_reverse_append).

        reverse_and_append(List, Tail) ->
            lists:re~verse(List) ++ Tail.
           "#,
            expect![[r#"
        -module(lists_reverse_append).

        reverse_and_append(List, Tail) ->
            lists:reverse(List, Tail).
           "#]],
        )
    }

    #[test]
    fn fixes_lists_reverse_append_with_complex_expressions() {
        check_fix(
            r#"
        //- /src/lists_reverse_append.erl
        -module(lists_reverse_append).

        process([H|T], Acc) ->
            lists:rev~erse([H|T]) ++ [1, 2, 3].
           "#,
            expect![[r#"
        -module(lists_reverse_append).

        process([H|T], Acc) ->
            lists:reverse([H|T], [1, 2, 3]).
           "#]],
        )
    }
}
