/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: inefficient_list_empty_check
//!
//! Detect patterns that use `length/1` to check if a list is empty/non-empty
//! and suggest using pattern matching directly on the list.
//!
//! Detected patterns:
//! - `case length(List) of 0 -> ...; _ -> ... end`
//! - `case length(List) =:= 0 of true -> ...; false/_ -> ... end`
//! - `case 0 =:= length(List) of true -> ...; false/_ -> ... end`
//! - `case length(List) =/= 0 of true -> ...; false/_ -> ... end`
//! - `case 0 =/= length(List) of true -> ...; false/_ -> ... end`
//! - `case length(List) > 0 of true -> ...; false/_ -> ... end`
//! - `case 0 < length(List) of true -> ...; false/_ -> ... end`
//! - `case length(List) >= 1 of true -> ...; false/_ -> ... end`
//! - `case 1 =< length(List) of true -> ...; false/_ -> ... end`

use elp_ide_assists::Assist;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct InefficientListEmptyCheckLinter;

impl Linter for InefficientListEmptyCheckLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::InefficientListEmptyCheck
    }

    fn description(&self) -> &'static str {
        "Use pattern matching on the list directly instead of checking length."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for InefficientListEmptyCheckLinter {
    type Context = ();

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> = vec![
                (
                    format!(
                        "ssr: case length({LIST_VAR}) of 0 -> {EMPTY_VAR}; _ -> {NON_EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) =:= 0 of true -> {EMPTY_VAR}; false -> {NON_EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) =:= 0 of true -> {EMPTY_VAR}; _ -> {NON_EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) =/= 0 of true -> {NON_EMPTY_VAR}; false -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) =/= 0 of true -> {NON_EMPTY_VAR}; _ -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) > 0 of true -> {NON_EMPTY_VAR}; false -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) > 0 of true -> {NON_EMPTY_VAR}; _ -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) >= 1 of true -> {NON_EMPTY_VAR}; false -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case length({LIST_VAR}) >= 1 of true -> {NON_EMPTY_VAR}; _ -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                // Reversed operand patterns
                (
                    format!(
                        "ssr: case 0 =:= length({LIST_VAR}) of true -> {EMPTY_VAR}; false -> {NON_EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case 0 =:= length({LIST_VAR}) of true -> {EMPTY_VAR}; _ -> {NON_EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case 0 =/= length({LIST_VAR}) of true -> {NON_EMPTY_VAR}; false -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case 0 =/= length({LIST_VAR}) of true -> {NON_EMPTY_VAR}; _ -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case 0 < length({LIST_VAR}) of true -> {NON_EMPTY_VAR}; false -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case 0 < length({LIST_VAR}) of true -> {NON_EMPTY_VAR}; _ -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case 1 =< length({LIST_VAR}) of true -> {NON_EMPTY_VAR}; false -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case 1 =< length({LIST_VAR}) of true -> {NON_EMPTY_VAR}; _ -> {EMPTY_VAR} end."
                    ),
                    (),
                ),
            ];
        }
        &PATTERNS
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let list_match = matched.placeholder_text(sema, LIST_VAR)?;
        let empty_match = matched.placeholder_text(sema, EMPTY_VAR)?;
        let non_empty_match = matched.placeholder_text(sema, NON_EMPTY_VAR)?;
        let mut builder = SourceChangeBuilder::new(matched.range.file_id);
        let replacement =
            format!("case {list_match} of [] -> {empty_match}; [_ | _] -> {non_empty_match} end");
        let range = matched.range.range;
        builder.replace(range, replacement);
        let fixes = vec![fix(
            "inefficient_list_empty_check",
            "Pattern match on list structure instead",
            builder.finish(),
            range,
        )];
        Some(fixes)
    }
}

pub(crate) static LINTER: InefficientListEmptyCheckLinter = InefficientListEmptyCheckLinter;

static LIST_VAR: &str = "_@List";
static EMPTY_VAR: &str = "_@Empty";
static NON_EMPTY_VAR: &str = "_@NonEmpty";

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::InefficientListEmptyCheck
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
    fn detects_list_empty_check_of() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case length(List) of 0 -> empty; _ -> non_empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn ignores_non_length_case() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_foo(List) ->
            case foo(List) of 0 -> empty; _ -> non_empty end.
           "#,
        )
    }

    #[test]
    fn ignores_list_empty_check_with_multiple_clauses() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_length(List) ->
            case length(List) of 0 -> empty; 1 -> singleton; _ -> many end.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_of() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) of 0 -> empty; _ -> non_empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn fixes_list_empty_check_of_multiline() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) of
                0 -> empty;
                _ -> non_empty
            end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn detects_list_empty_check_eq_zero() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case length(List) =:= 0 of true -> empty; false -> non_empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn detects_list_empty_check_eq_zero_wildcard() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case length(List) =:= 0 of true -> empty; _ -> non_empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_eq_zero() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) =:= 0 of true -> empty; false -> non_empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn fixes_list_empty_check_eq_zero_multiline() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) =:= 0 of
                true -> empty;
                false -> non_empty
            end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn detects_list_empty_check_ne_zero() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case length(List) =/= 0 of true -> non_empty; false -> empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_ne_zero() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) =/= 0 of true -> non_empty; false -> empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn fixes_list_empty_check_ne_zero_multiline() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) =/= 0 of
                true -> non_empty;
                false -> empty
            end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn detects_list_empty_check_gt_zero() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case length(List) > 0 of true -> non_empty; false -> empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_gt_zero() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) > 0 of true -> non_empty; false -> empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn fixes_list_empty_check_gt_zero_multiline() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) > 0 of
                true -> non_empty;
                false -> empty
            end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn detects_list_empty_check_ge_one() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case length(List) >= 1 of true -> non_empty; false -> empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_ge_one() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) >= 1 of true -> non_empty; false -> empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn fixes_list_empty_check_ge_one_multiline() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case len~gth(List) >= 1 of
                true -> non_empty;
                false -> empty
            end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    // T256431185: SSR can return matches with file_id from header files
    // when the pattern is used in a macro argument. Only process matches
    // that belong to the current file being analyzed.
    #[test]
    fn ignores_match_in_macro_arg_from_header_file() {
        check_diagnostics(
            r#"
        //- /assert/include/assert.hrl app:assert include_path:/assert/include
        %% Padding to ensure header file range exceeds source file length
        %% This triggers file_id mismatch when SSR matches in macro arg
        -define(assert_result(Expr),
            case (Expr) of
                ok -> ok;
                _ -> erlang:error(assertion_failed)
            end).

        //- /src/test.erl app:test
        -module(test).
        -include_lib("assert/include/assert.hrl").
        f(List) ->
            ?assert_result(case length(List) > 0 of true -> ok; false -> err end).
           "#,
        )
    }

    // ---- Reversed operand tests ----

    #[test]
    fn detects_list_empty_check_zero_eq() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 0 =:= length(List) of true -> empty; false -> non_empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn detects_list_empty_check_zero_eq_wildcard() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 0 =:= length(List) of true -> empty; _ -> non_empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_zero_eq() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 0 =:= len~gth(List) of true -> empty; false -> non_empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn detects_list_empty_check_zero_ne() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 0 =/= length(List) of true -> non_empty; false -> empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_zero_ne() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 0 =/= len~gth(List) of true -> non_empty; false -> empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn detects_list_empty_check_zero_lt() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 0 < length(List) of true -> non_empty; false -> empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_zero_lt() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 0 < len~gth(List) of true -> non_empty; false -> empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }

    #[test]
    fn detects_list_empty_check_one_le() {
        check_diagnostics(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 1 =< length(List) of true -> non_empty; false -> empty end.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0065: Use pattern matching on the list directly instead of checking length.
           "#,
        )
    }

    #[test]
    fn fixes_list_empty_check_one_le() {
        check_fix(
            r#"
        //- /src/list_empty_check.erl
        -module(list_empty_check).

        check_list(List) ->
            case 1 =< len~gth(List) of true -> non_empty; false -> empty end.
           "#,
            expect![[r#"
        -module(list_empty_check).

        check_list(List) ->
            case List of [] -> empty; [_ | _] -> non_empty end.
           "#]],
        )
    }
}
