/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: simplify_negation
//!
//! offer to rewrite matching on the negation of a boolean expression to matching
//! on negated pattern literals. e.g.
//!
//! e.g.
//!
//! ```ignore
//! case not A of
//!     true -> AIsFalseBranch;
//!     false -> AIsTrueBranch
//! end
//! ```
//!
//! becomes
//!
//! ```ignore
//! case A of
//!     false -> AIsFalseBranch;
//!     true -> AIsTrueBranch
//! end
//! ```

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::text_edit::TextRange;
use elp_ide_ssr::Match;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

static NEGATED_EXPR_VAR: &str = "_@NegatedExpr";
static AFFIRMATIVE_BRANCH_VAR: &str = "_@AffirmativeBranch";
static NEGATIVE_BRANCH_VAR: &str = "_@NegativeBranch";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BranchOrder {
    AffirmativeBranchFirst,
    NegativeBranchFirst,
}

pub(crate) struct SimplifyNegationLinter;

impl Linter for SimplifyNegationLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::SimplifyNegation
    }

    fn description(&self) -> &'static str {
        "Consider rewriting to match directly on the negated expression."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Information
    }
}

impl SsrPatternsLinter for SimplifyNegationLinter {
    type Context = BranchOrder;

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, BranchOrder)> = vec![
                (
                    format!(
                        "ssr: case not {NEGATED_EXPR_VAR} of
                            true -> {NEGATIVE_BRANCH_VAR};
                            false -> {AFFIRMATIVE_BRANCH_VAR}
                          end."
                    ),
                    BranchOrder::NegativeBranchFirst,
                ),
                (
                    format!(
                        "ssr: case not {NEGATED_EXPR_VAR} of
                            false -> {AFFIRMATIVE_BRANCH_VAR};
                            true -> {NEGATIVE_BRANCH_VAR}
                          end."
                    ),
                    BranchOrder::AffirmativeBranchFirst,
                ),
                (
                    format!(
                        "ssr: case not {NEGATED_EXPR_VAR} of
                            true -> {NEGATIVE_BRANCH_VAR};
                            _ -> {AFFIRMATIVE_BRANCH_VAR}
                          end."
                    ),
                    BranchOrder::NegativeBranchFirst,
                ),
                (
                    format!(
                        "ssr: case not {NEGATED_EXPR_VAR} of
                            false -> {AFFIRMATIVE_BRANCH_VAR};
                            _ -> {NEGATIVE_BRANCH_VAR}
                          end."
                    ),
                    BranchOrder::AffirmativeBranchFirst,
                ),
                (
                    format!(
                        "ssr: if not {NEGATED_EXPR_VAR} -> {NEGATIVE_BRANCH_VAR};
                            true -> {AFFIRMATIVE_BRANCH_VAR}
                          end."
                    ),
                    BranchOrder::NegativeBranchFirst,
                ),
            ];
        }
        &PATTERNS
    }

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn range(&self, sema: &Semantic, matched: &Match) -> Option<TextRange> {
        matched.placeholder_range(sema, NEGATED_EXPR_VAR)
    }

    fn fixes(
        &self,
        context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        if let Some(comments) = matched.comments(sema)
            && !comments.is_empty()
        {
            return None;
        }
        let old_conditional_range = matched.range.range;
        let replacement_text = get_replacement(sema, matched, *context)?;
        let mut builder = SourceChangeBuilder::new(file_id);
        builder.replace(old_conditional_range, replacement_text);
        Some(vec![fix(
            "simplify_negation",
            "Rewrite to match directly on the negated expression",
            builder.finish(),
            old_conditional_range,
        )])
    }
}

pub(crate) static LINTER: SimplifyNegationLinter = SimplifyNegationLinter;

fn get_replacement(sema: &Semantic, m: &Match, branch_order: BranchOrder) -> Option<String> {
    let new_discriminee_expr_text = m.placeholder_text(sema, NEGATED_EXPR_VAR)?;
    let affirmative_branch_text = m.placeholder_text(sema, AFFIRMATIVE_BRANCH_VAR)?;
    let negative_branch_text = m.placeholder_text(sema, NEGATIVE_BRANCH_VAR)?;
    // Preserve branch arm order to minimise diff churn
    match branch_order {
        BranchOrder::AffirmativeBranchFirst => Some(format!(
            "case {new_discriminee_expr_text} of true -> {affirmative_branch_text}; false -> {negative_branch_text} end"
        )),
        BranchOrder::NegativeBranchFirst => Some(format!(
            "case {new_discriminee_expr_text} of false -> {negative_branch_text}; true -> {affirmative_branch_text} end"
        )),
    }
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::SimplifyNegation
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
    fn detects_negation_in_match_true_pattern_first() {
        check_diagnostics(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            case not A of true -> FalseBranch; false -> TrueBranch end.
         %%          ^ 💡 information: W0044: Consider rewriting to match directly on the negated expression.
            "#,
        )
    }

    #[test]
    fn detects_negation_in_match_false_pattern_first() {
        check_diagnostics(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            case not A of false -> TrueBranch; true -> FalseBranch end.
         %%          ^ 💡 information: W0044: Consider rewriting to match directly on the negated expression.
            "#,
        )
    }

    #[test]
    fn detects_negation_in_if_expression() {
        check_diagnostics(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            if not A -> FalseBranch; true -> TrueBranch end.
         %%        ^ 💡 information: W0044: Consider rewriting to match directly on the negated expression.
            "#,
        )
    }

    #[test]
    fn ignores_nested_negation() {
        check_diagnostics(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, F, TrueBranch, FalseBranch) ->
            if F(not A) -> FalseBranch; true -> TrueBranch end.
            "#,
        )
    }

    #[test]
    fn rewrites_negation_in_match_true_pattern_first() {
        check_fix(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            case not ~A of true -> FalseBranch; false -> TrueBranch end.
         "#,
            expect![[r#"
                -module(simplify_negation).

                fn(A, TrueBranch, FalseBranch) ->
                   case A of false -> FalseBranch; true -> TrueBranch end.
                "#]],
        )
    }

    #[test]
    fn rewrites_negation_in_match_true_pattern_then_wildcard() {
        check_fix(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            case not ~A of true -> FalseBranch; _ -> TrueBranch end.
         "#,
            expect![[r#"
                -module(simplify_negation).

                fn(A, TrueBranch, FalseBranch) ->
                   case A of false -> FalseBranch; true -> TrueBranch end.
                "#]],
        )
    }

    #[test]
    fn rewrites_negation_in_match_false_pattern_then_wildcard() {
        check_fix(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            case not ~A of false -> TrueBranch; _ -> FalseBranch end.
         "#,
            expect![[r#"
                -module(simplify_negation).

                fn(A, TrueBranch, FalseBranch) ->
                   case A of true -> TrueBranch; false -> FalseBranch end.
                "#]],
        )
    }

    #[test]
    fn rewrites_negation_in_if_expression() {
        check_fix(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            if not ~A -> FalseBranch; true -> TrueBranch end.
         "#,
            expect![[r#"
                -module(simplify_negation).

                fn(A, TrueBranch, FalseBranch) ->
                   case A of false -> FalseBranch; true -> TrueBranch end.
                "#]],
        )
    }

    #[test]
    fn preserves_branch_order_to_minimise_churn() {
        check_fix(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            case not ~A of false -> TrueBranch; true -> FalseBranch end.
         "#,
            expect![[r#"
                -module(simplify_negation).

                fn(A, TrueBranch, FalseBranch) ->
                   case A of true -> TrueBranch; false -> FalseBranch end.
                "#]],
        );
        check_fix(
            r#"
         //- /src/simplify_negation.erl
         -module(simplify_negation).

         fn(A, TrueBranch, FalseBranch) ->
            case not ~A of true -> FalseBranch; false -> TrueBranch end.
         "#,
            expect![[r#"
                -module(simplify_negation).

                fn(A, TrueBranch, FalseBranch) ->
                   case A of false -> FalseBranch; true -> TrueBranch end.
                "#]],
        )
    }
}
