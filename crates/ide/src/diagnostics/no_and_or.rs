/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: no_and_or
//!
//! Detect usages of `and` and `or` operators and suggest using `andalso` and `orelse` instead.

use elp_ide_assists::Assist;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_syntax::AstNode;
use elp_syntax::SyntaxKind;
use elp_syntax::TextRange;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::diagnostics::Linter;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorKind {
    And,
    Or,
}

pub(crate) struct NoAndOrLinter;

impl Linter for NoAndOrLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoAndOr
    }

    fn description(&self) -> &'static str {
        "Use `andalso`/`orelse` instead of `and`/`or`. OTP 29 will warn for `and`/`or` usage."
    }
}

impl SsrPatternsLinter for NoAndOrLinter {
    type Context = OperatorKind;

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, OperatorKind)> = vec![
                (format!("ssr: {LHS} and {RHS}."), OperatorKind::And,),
                (format!("ssr: {LHS} or {RHS}."), OperatorKind::Or),
            ];
        }
        &PATTERNS
    }

    fn pattern_description(&self, context: &Self::Context) -> &'static str {
        match context {
            OperatorKind::And => {
                "Use `andalso` instead of `and`. OTP 29 will warn for `and` usage."
            }
            OperatorKind::Or => "Use `orelse` instead of `or`. OTP 29 will warn for `or` usage.",
        }
    }

    fn range(&self, sema: &Semantic, matched: &Match) -> Option<TextRange> {
        let source_file = sema.parse(matched.range.file_id);
        let syntax = source_file.value.syntax();
        let node = syntax.covering_element(matched.range.range);

        // Find the operator token (ANON_AND or ANON_OR) within the matched range
        let operator_token = node
            .as_node()
            .into_iter()
            .flat_map(|n| n.descendants_with_tokens())
            .filter_map(|element| element.into_token())
            .find(|token| matches!(token.kind(), SyntaxKind::ANON_AND | SyntaxKind::ANON_OR))?;

        Some(operator_token.text_range())
    }

    fn fixes(
        &self,
        context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let left_match = matched.placeholder_text(sema, LHS)?;
        let right_match = matched.placeholder_text(sema, RHS)?;

        let (replacement_op, fix_id, fix_label) = match context {
            OperatorKind::And => ("andalso", "and_to_andalso", "Replace `and` with `andalso`"),
            OperatorKind::Or => ("orelse", "or_to_orelse", "Replace `or` with `orelse`"),
        };

        let mut builder = SourceChangeBuilder::new(matched.range.file_id);
        let replacement = format!("{left_match} {replacement_op} {right_match}");
        let range = matched.range.range;
        builder.replace(range, replacement);
        let fixes = vec![fix(fix_id, fix_label, builder.finish(), range)];
        Some(fixes)
    }
}

pub(crate) static LINTER: NoAndOrLinter = NoAndOrLinter;

static LHS: &str = "_@LHS";
static RHS: &str = "_@RHS";

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::NoAndOr
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
    fn detects_and_operator() {
        check_diagnostics(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_and(A, B) ->
             A and B.
         %%    ^^^ 💡 warning: W0069: Use `andalso` instead of `and`. OTP 29 will warn for `and` usage.
            "#,
        )
    }

    #[test]
    fn detects_or_operator() {
        check_diagnostics(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_or(A, B) ->
             A or B.
          %%   ^^ 💡 warning: W0069: Use `orelse` instead of `or`. OTP 29 will warn for `or` usage.
            "#,
        )
    }

    #[test]
    fn detects_and_in_complex_expression() {
        check_diagnostics(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_complex(A, B, C) ->
             (A > 0) and (B < 10) andalso C.
         %%          ^^^ 💡 warning: W0069: Use `andalso` instead of `and`. OTP 29 will warn for `and` usage.
            "#,
        )
    }

    #[test]
    fn detects_or_in_complex_expression() {
        check_diagnostics(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_complex(A, B, C) ->
             (A > 0) or (B < 10) orelse C.
          %%         ^^ 💡 warning: W0069: Use `orelse` instead of `or`. OTP 29 will warn for `or` usage.
            "#,
        )
    }

    #[test]
    fn ignores_andalso_operator() {
        check_diagnostics(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_andalso(A, B) ->
             A andalso B.
            "#,
        )
    }

    #[test]
    fn ignores_orelse_operator() {
        check_diagnostics(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_orelse(A, B) ->
             A orelse B.
            "#,
        )
    }

    #[test]
    fn fixes_and_to_andalso() {
        check_fix(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_and(A, B) ->
             A a~nd B.
            "#,
            expect![[r#"
         -module(and_or_test).

         test_and(A, B) ->
             A andalso B.
            "#]],
        )
    }

    #[test]
    fn fixes_or_to_orelse() {
        check_fix(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_or(A, B) ->
             A o~r B.
            "#,
            expect![[r#"
         -module(and_or_test).

         test_or(A, B) ->
             A orelse B.
            "#]],
        )
    }

    #[test]
    fn fixes_and_with_complex_expressions() {
        check_fix(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_complex(X, Y) ->
             is_integer(X) a~nd (Y > 0).
            "#,
            expect![[r#"
         -module(and_or_test).

         test_complex(X, Y) ->
             is_integer(X) andalso (Y > 0).
            "#]],
        )
    }

    #[test]
    fn fixes_or_with_function_calls() {
        check_fix(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_fn_calls(X, Y) ->
             validate(X) o~r fallback(Y).
            "#,
            expect![[r#"
         -module(and_or_test).

         test_fn_calls(X, Y) ->
             validate(X) orelse fallback(Y).
            "#]],
        )
    }

    #[test]
    fn detects_multiple_and_or_in_same_function() {
        check_diagnostics(
            r#"
         //- /src/and_or_test.erl
         -module(and_or_test).

         test_multiple(A, B, C) ->
             X = A and B,
         %%        ^^^ 💡 warning: W0069: Use `andalso` instead of `and`. OTP 29 will warn for `and` usage.
             Y = B or C,
         %%        ^^ 💡 warning: W0069: Use `orelse` instead of `or`. OTP 29 will warn for `or` usage.
             X andalso Y.
            "#,
        )
    }

    // T256431185 ELP currently returns the incorrect file_id for the operator
    // used in the macro argument, which can lead to a crash due to a non existent offset.
    #[test]
    fn or_in_macro_argument_from_header_file() {
        check_diagnostics(
            r#"
         //- /assert/include/assert.hrl app:assert include_path:/assert/include
         %% Padding to ensure header file range exceeds source file length
         %% This triggers file_id mismatch when SSR matches 'or' in macro arg
         -define(assert(BoolExpr),
             case (BoolExpr) of
                 true -> ok;
                 _ -> erlang:error(assertion_failed)
             end).

         //- /src/test.erl app:test
         -module(test).
         -include_lib("assert/include/assert.hrl").
         f(A, B) ->
             ?assert((A >= 0) or (B =< 2)).
            "#,
        )
    }
}
