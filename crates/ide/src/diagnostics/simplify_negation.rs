/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::match_pattern_in_file_functions;
use hir::Semantic;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::fix;

static NEGATED_EXPR_VAR: &str = "_@NegatedExpr";
static AFFIRMATIVE_BRANCH_VAR: &str = "_@AffirmativeBranch";
static NEGATIVE_BRANCH_VAR: &str = "_@NegativeBranch";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BranchOrder {
    AffirmativeBranchFirst,
    NegativeBranchFirst,
}

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|acc, sema, file_id, _ext| {
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case not {NEGATED_EXPR_VAR} of
                    true -> {NEGATIVE_BRANCH_VAR};
                    false -> {AFFIRMATIVE_BRANCH_VAR}
                  end."
            )
            .as_str(),
            BranchOrder::NegativeBranchFirst,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case not {NEGATED_EXPR_VAR} of
                    false -> {AFFIRMATIVE_BRANCH_VAR};
                    true -> {NEGATIVE_BRANCH_VAR}
                  end."
            )
            .as_str(),
            BranchOrder::AffirmativeBranchFirst,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case not {NEGATED_EXPR_VAR} of
                    true -> {NEGATIVE_BRANCH_VAR};
                    _ -> {AFFIRMATIVE_BRANCH_VAR}
                  end."
            )
            .as_str(),
            BranchOrder::NegativeBranchFirst,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case not {NEGATED_EXPR_VAR} of
                    false -> {AFFIRMATIVE_BRANCH_VAR};
                    _ -> {NEGATIVE_BRANCH_VAR}
                  end."
            )
            .as_str(),
            BranchOrder::AffirmativeBranchFirst,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: if not {NEGATED_EXPR_VAR} -> {NEGATIVE_BRANCH_VAR};
                    true -> {AFFIRMATIVE_BRANCH_VAR}
                  end."
            )
            .as_str(),
            BranchOrder::NegativeBranchFirst,
        );
    },
};

fn from_ssr(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    ssr_pattern: &str,
    branch_order: BranchOrder,
) {
    let matches = match_pattern_in_file_functions(
        sema,
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        ssr_pattern,
    );
    matches.matches.iter().for_each(|m| {
        if let Some(diagnostic) = make_diagnostic(sema, m, branch_order) {
            diags.push(diagnostic)
        }
    });
}

fn make_diagnostic(
    sema: &Semantic,
    matched: &Match,
    branch_order: BranchOrder,
) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let old_conditional_range = matched.range.range;
    let squiggly_range = matched.placeholder_range(sema, NEGATED_EXPR_VAR)?;
    let message = "Consider rewriting to match directly on the negated expression.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let equality_pattern_replacement = get_replacement(sema, file_id, matched, branch_order)?;
    builder.replace(old_conditional_range, equality_pattern_replacement);
    let fixes = vec![fix(
        "simplify_negation",
        "Rewrite to match directly on the negated expression",
        builder.finish(),
        old_conditional_range,
    )];
    Some(
        Diagnostic::new(DiagnosticCode::SimplifyNegation, message, squiggly_range)
            .with_severity(Severity::Information)
            .with_ignore_fix(sema, file_id)
            .with_fixes(Some(fixes)),
    )
}

fn get_replacement(
    sema: &Semantic,
    original_file_id: FileId,
    m: &Match,
    branch_order: BranchOrder,
) -> Option<String> {
    if let Some(comments) = m.comments(sema) {
        // Avoid clobbering comments in the original source code
        if !comments.is_empty() {
            return None;
        }
    }
    if m.range.file_id != original_file_id {
        // We've somehow ended up with a match in a different file - this means we've
        // accidentally expanded a macro from a different file, or some other complex case that
        // gets hairy, so bail out.
        return None;
    }
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
         %%          ^ 💡 information: Consider rewriting to match directly on the negated expression.
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
         %%          ^ 💡 information: Consider rewriting to match directly on the negated expression.
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
         %%        ^ 💡 information: Consider rewriting to match directly on the negated expression.
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
