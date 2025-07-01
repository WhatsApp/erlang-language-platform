/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: equality_check_with_unnecessary_operator
//!
//! offer to rewrite simple equality checks which pattern match on the result
//! of the equality operator to use an equality pattern directly
//!
//! e.g.
//!
//! ```ignore
//! case A =:= foo of
//!     true -> Same;
//!     false -> Different
//! end
//! ```
//!
//! becomes
//!
//! ```ignore
//! case A of
//!     foo -> Same;
//!     _ -> Different
//! end
//! ```

use std::cmp::Ordering;
use std::cmp::max;
use std::fmt::Debug;

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::PlaceholderMatch;
use elp_ide_ssr::SubId;
use elp_ide_ssr::match_pattern_in_file_functions;
use hir::AnyExprId;
use hir::BinarySeg;
use hir::Body;
use hir::Expr;
use hir::Pat;
use hir::Semantic;
use hir::Var;
use hir::db::InternDatabase;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;

use crate::diagnostics::Category;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::fix;

static LHS_VAR: &str = "_@Lhs";
static RHS_VAR: &str = "_@Rhs";
static SAME_BRANCH_VAR: &str = "_@SameBranch";
static DIFF_BRANCH_VAR: &str = "_@DiffBranch";
static UNDERSCORE_VAR: &str = "_@Wildcard";

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|acc, sema, file_id, _ext| {
        // N.B. we only apply when the operator is `=:=`/`=/=` (not `==`/`/=`)
        // as we can't be sure that the values with be safe to match otherwise
        // (e.g. floats are not safely matchable)
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =:= {RHS_VAR} of
                    true -> {SAME_BRANCH_VAR};
                    false -> {DIFF_BRANCH_VAR}
                  end."
            )
            .as_str(),
            false,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =/= {RHS_VAR} of
                    true -> {DIFF_BRANCH_VAR};
                    false -> {SAME_BRANCH_VAR}
                  end."
            )
            .as_str(),
            false,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: if {LHS_VAR} =/= {RHS_VAR} -> {DIFF_BRANCH_VAR};
                    true -> {SAME_BRANCH_VAR}
                  end."
            )
            .as_str(),
            false,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: if {LHS_VAR} =:= {RHS_VAR} -> {SAME_BRANCH_VAR};
                    true -> {DIFF_BRANCH_VAR}
                  end."
            )
            .as_str(),
            false,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =:= {RHS_VAR} of
                    false -> {DIFF_BRANCH_VAR};
                    true -> {SAME_BRANCH_VAR}
                  end."
            )
            .as_str(),
            false,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =/= {RHS_VAR} of
                    false -> {SAME_BRANCH_VAR};
                    true -> {DIFF_BRANCH_VAR}
                  end."
            )
            .as_str(),
            false,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =:= {RHS_VAR} of
                    true -> {SAME_BRANCH_VAR};
                    {UNDERSCORE_VAR} -> {DIFF_BRANCH_VAR}
                  end."
            )
            .as_str(),
            true,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =/= {RHS_VAR} of
                    true -> {DIFF_BRANCH_VAR};
                    {UNDERSCORE_VAR} -> {SAME_BRANCH_VAR}
                  end."
            )
            .as_str(),
            true,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =:= {RHS_VAR} of
                    false -> {DIFF_BRANCH_VAR};
                    {UNDERSCORE_VAR} -> {SAME_BRANCH_VAR}
                  end."
            )
            .as_str(),
            true,
        );
        from_ssr(
            acc,
            sema,
            file_id,
            format!(
                "ssr: case {LHS_VAR} =/= {RHS_VAR} of
                    false -> {SAME_BRANCH_VAR};
                    {UNDERSCORE_VAR} -> {DIFF_BRANCH_VAR}
                  end."
            )
            .as_str(),
            true,
        );
    },
};

fn from_ssr(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    ssr_pattern: &str,
    underscore_expected: bool,
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
        validate_and_make_diagnostic(diags, sema, file_id, m, underscore_expected);
    });
}

fn validate_and_make_diagnostic(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic<'_>,
    file_id: FileId,
    m: &Match,
    underscore_expected: bool,
) -> Option<()> {
    let lhs = &m.get_placeholder_match(sema, LHS_VAR)?;
    let rhs = &m.get_placeholder_match(sema, RHS_VAR)?;
    let wildcard = &m.get_placeholder_match(sema, UNDERSCORE_VAR);
    let body_arc = m.matched_node_body.get_body(sema)?;
    let body = body_arc.as_ref();
    if check_underscore(sema, body, wildcard, underscore_expected) {
        // As long of one of the expressions is a valid pattern, we can rewrite to use it in the
        // pattern position, and use the other in the expression position.
        // We prioritise literals and more complex values in the pattern position, because it is more idiomatic.
        let lhs_as_pattern_priority = pattern_priority(sema, body, lhs);
        let rhs_as_pattern_priority = pattern_priority(sema, body, rhs);
        match (lhs_as_pattern_priority, rhs_as_pattern_priority) {
            (Some(left), Some(right)) => match left.cmp(&right) {
                Ordering::Less => report_diagnostic(diags, sema, file_id, m, body, lhs, rhs),
                Ordering::Greater => report_diagnostic(diags, sema, file_id, m, body, rhs, lhs),
                Ordering::Equal => report_diagnostic(diags, sema, file_id, m, body, lhs, rhs),
            },
            (Some(_), None) => report_diagnostic(diags, sema, file_id, m, body, rhs, lhs),
            (None, Some(_)) => report_diagnostic(diags, sema, file_id, m, body, lhs, rhs),
            (None, None) => {
                // No rewrite possible - neither side of the operator is valid in the pattern position
            }
        }
    }
    Some(())
}

fn report_diagnostic(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic<'_>,
    file_id: FileId,
    m: &Match,
    body: &Body,
    discriminee: &PlaceholderMatch,
    pattern: &PlaceholderMatch,
) {
    if let Some(diagnostic) = make_diagnostic(sema, file_id, m, body, discriminee, pattern) {
        diags.push(diagnostic)
    }
}

// If the wildcard metavar was bound at all, make sure it is equivalent to an underscore
fn check_underscore(
    sema: &Semantic,
    body: &Body,
    wildcard: &Option<PlaceholderMatch>,
    underscore_expected: bool,
) -> bool {
    if underscore_expected {
        is_underscore(sema, body, wildcard)
    } else {
        true
    }
}

fn is_underscore(sema: &Semantic, body: &Body, wildcard: &Option<PlaceholderMatch>) -> bool {
    match wildcard {
        Some(m) => match m.code_id {
            SubId::AnyExprId(expr_id) => match expr_id {
                AnyExprId::Expr(expr_id) => match body.exprs[expr_id] {
                    Expr::Var(var) => is_wildcard(sema, var),
                    _ => false,
                },
                AnyExprId::Pat(pat_id) => match body.pats[pat_id] {
                    Pat::Var(var) => is_wildcard(sema, var),
                    _ => false,
                },
                _ => false,
            },
            SubId::Var(var) => is_wildcard(sema, var),
            _ => false,
        },
        None => false,
    }
}

fn is_wildcard(sema: &Semantic, var: Var) -> bool {
    var.as_string(sema.db.upcast()).starts_with("_")
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct PatternPriority {
    num_vars_used: usize,
    nesting_depth: usize,
}

fn coalesce_children(
    maybe_a: Option<PatternPriority>,
    maybe_b: &Option<PatternPriority>,
) -> Option<PatternPriority> {
    let a = maybe_a.clone()?;
    let b = maybe_b.clone()?;
    Some(PatternPriority {
        num_vars_used: a.num_vars_used + b.num_vars_used,
        nesting_depth: max(a.nesting_depth, b.nesting_depth),
    })
}

fn nest(maybe_p: Option<PatternPriority>) -> Option<PatternPriority> {
    let p = maybe_p.clone()?;
    Some(PatternPriority {
        nesting_depth: p.nesting_depth + 1,
        ..p
    })
}

fn coalesce_all_nested_children(
    nested_children: &[Option<PatternPriority>],
) -> Option<PatternPriority> {
    let init = Some(PatternPriority {
        num_vars_used: 0,
        nesting_depth: 0,
    });
    nest(nested_children.iter().fold(init, coalesce_children))
}

impl PartialOrd for PatternPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for PatternPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .num_vars_used
            .cmp(&self.num_vars_used)
            .then(self.nesting_depth.cmp(&other.nesting_depth))
    }
}

// Returns None if the match is not a valid pattern
fn pattern_priority(
    sema: &Semantic,
    body: &Body,
    matched: &PlaceholderMatch,
) -> Option<PatternPriority> {
    let pp = match matched.code_id {
        SubId::AnyExprId(AnyExprId::Expr(expr_id)) => {
            pattern_priority_expr(sema, body, body.exprs[expr_id].clone())
        }
        SubId::AnyExprId(AnyExprId::Pat(pat_id)) => {
            pattern_priority_pat(sema, body, body.pats[pat_id].clone())
        }
        SubId::AnyExprId(AnyExprId::TypeExpr(_)) => None,
        SubId::AnyExprId(AnyExprId::Term(_)) => None,
        SubId::Atom(_) => Some(PatternPriority {
            num_vars_used: 0,
            nesting_depth: 0,
        }),
        SubId::Var(_) => None, // Don't use raw vars in patterns
        SubId::UnaryOp(_) => None,
        SubId::BinaryOp(_) => None,
        SubId::MapOp(_) => None,
        SubId::Constant(_) => None, // Being conservative here
    };
    // We don't allow bare variables as patterns, since it can make refactorings less safe in the future.
    // In particular, it means a rename can change an equality pattern into a binding, quietly changing
    // semantics quite drastically.
    let bare_var = Some(PatternPriority {
        num_vars_used: 1,
        nesting_depth: 0,
    });
    if pp == bare_var { None } else { pp }
}

fn pattern_priority_expr(sema: &Semantic, body: &Body, expr: Expr) -> Option<PatternPriority> {
    match expr {
        Expr::Var(_) => Some(PatternPriority {
            num_vars_used: 1,
            nesting_depth: 0,
        }),
        Expr::Literal(_) => Some(PatternPriority {
            num_vars_used: 0,
            nesting_depth: 0,
        }),
        Expr::Tuple { exprs } => coalesce_all_nested_children(
            &exprs
                .iter()
                .map(|elem| pattern_priority_expr(sema, body, body.exprs[*elem].clone()))
                .collect::<Vec<_>>(),
        ),
        Expr::List { exprs, tail } => coalesce_all_nested_children(
            &exprs
                .iter()
                .chain(tail.iter())
                .map(|elem| pattern_priority_expr(sema, body, body.exprs[*elem].clone()))
                .collect::<Vec<_>>(),
        ),
        Expr::UnaryOp { expr, op: _ } => {
            nest(pattern_priority_expr(sema, body, body.exprs[expr].clone()))
        }
        Expr::Record { name: _, fields } => coalesce_all_nested_children(
            &fields
                .iter()
                .map(|(_, field)| pattern_priority_expr(sema, body, body.exprs[*field].clone()))
                .collect::<Vec<_>>(),
        ),
        Expr::Paren { expr } => pattern_priority_expr(sema, body, body.exprs[expr].clone()),
        Expr::Binary { segs } => {
            if is_seg_formulation_valid_in_pattern(sema, &segs) {
                coalesce_all_nested_children(
                    &segs
                        .iter()
                        .map(|seg| pattern_priority_expr(sema, body, body.exprs[seg.elem].clone()))
                        .collect::<Vec<_>>(),
                )
            } else {
                None
            }
        }
        Expr::Map { .. } => None, // Map's expression semantics are different to their pattern semantics (e.g. patterns need match only a subset of the map value, so we explicitly reject them here for simplicity)
        _ => None,
    }
}

fn is_seg_formulation_valid_in_pattern<T: Debug>(sema: &Semantic, segs: &[BinarySeg<T>]) -> bool {
    // A binary field without size is only allowed at the end of a binary pattern
    if segs.len() > 1 {
        let intern_db: &dyn InternDatabase = sema.db.upcast();
        let without_last = segs.iter().take(segs.len() - 1).collect::<Vec<_>>();
        without_last.iter().all(|seg| {
            if seg
                .tys
                .iter()
                .any(|ty: &hir::Atom| ty.as_string(intern_db) == "binary")
            {
                seg.size.is_some()
            } else {
                true
            }
        })
    } else {
        true
    }
}

fn pattern_priority_pat(sema: &Semantic, body: &Body, pat: Pat) -> Option<PatternPriority> {
    match pat {
        Pat::Binary { segs } => {
            if is_seg_formulation_valid_in_pattern(sema, &segs) {
                coalesce_all_nested_children(
                    &segs
                        .iter()
                        .map(|seg| pattern_priority_pat(sema, body, body.pats[seg.elem].clone()))
                        .collect::<Vec<_>>(),
                )
            } else {
                None
            }
        }
        Pat::Literal { .. } => Some(PatternPriority {
            num_vars_used: 0,
            nesting_depth: 0,
        }),
        Pat::Var { .. } => Some(PatternPriority {
            num_vars_used: 1,
            nesting_depth: 0,
        }),
        Pat::Tuple { pats } => coalesce_all_nested_children(
            &pats
                .iter()
                .map(|pat| pattern_priority_pat(sema, body, body.pats[*pat].clone()))
                .collect::<Vec<_>>(),
        ),
        Pat::List { pats, tail } => coalesce_all_nested_children(
            &pats
                .iter()
                .chain(tail.iter())
                .map(|pat| pattern_priority_pat(sema, body, body.pats[*pat].clone()))
                .collect::<Vec<_>>(),
        ),
        Pat::Map { .. } => None, // Map's expression semantics are different to their pattern semantics (e.g. patterns need match only a subset of the map value, so we explicitly reject them here for simplicity)
        Pat::UnaryOp { pat, op: _ } => {
            nest(pattern_priority_pat(sema, body, body.pats[pat].clone()))
        }
        Pat::Record { name: _, fields } => coalesce_all_nested_children(
            &fields
                .iter()
                .map(|(_, field)| pattern_priority_pat(sema, body, body.pats[*field].clone()))
                .collect::<Vec<_>>(),
        ),
        _ => None,
    }
}

fn make_diagnostic(
    sema: &Semantic,
    original_file_id: FileId,
    matched: &Match,
    body: &Body,
    expr: &PlaceholderMatch,
    pat: &PlaceholderMatch,
) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    if file_id != original_file_id {
        // We've somehow ended up with a match in a different file - this means we've
        // accidentally expanded a macro from a different file, or some other complex case that
        // gets hairy, so bail out.
        return None;
    }
    let old_match_range = matched.range.range;
    let discriminee_lhs_range = matched.placeholder_range(sema, LHS_VAR)?;
    let discriminee_rhs_range = matched.placeholder_range(sema, RHS_VAR)?;
    let discriminee_range = discriminee_lhs_range.cover(discriminee_rhs_range);
    let message = "Consider rewriting to an equality match.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let equality_pattern_replacement = get_replacement(sema, matched, body, expr, pat)?;
    builder.replace(old_match_range, equality_pattern_replacement);
    let fixes = vec![fix(
        "equality_check_with_unnecessary_operator",
        "Rewrite to use equality match",
        builder.finish(),
        old_match_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::EqualityCheckWithUnnecessaryOperator,
            message,
            discriminee_range,
        )
        .with_severity(Severity::Information)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes))
        .add_categories([Category::SimplificationRule]),
    )
}

fn get_replacement(
    sema: &Semantic,
    m: &Match,
    body: &Body,
    expr: &PlaceholderMatch,
    pat: &PlaceholderMatch,
) -> Option<String> {
    if let Some(comments) = m.comments(sema) {
        // Avoid clobbering comments in the original source code
        if !comments.is_empty() {
            return None;
        }
    }
    let same_branch_text = m.placeholder_text(sema, SAME_BRANCH_VAR)?;
    let diff_branch_text = m.placeholder_text(sema, DIFF_BRANCH_VAR)?;
    let expr_text = expr.text(sema, body)?;
    let pat_text = pat.text(sema, body)?;
    Some(format!(
        "case {expr_text} of {pat_text} -> {same_branch_text}; _ -> {diff_branch_text} end"
    ))
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::EqualityCheckWithUnnecessaryOperator
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
    fn detects_when_different_case_matches_on_true_and_false() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =:= b of true -> Same; false -> Diff end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn detects_when_different_case_matches_on_false_and_true() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =:= b of false -> Diff; true -> Same end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn ignores_when_both_lhs_and_rhs_are_bindings() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =:= B of true -> Same; false -> Diff end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, B, Same, Diff) ->
            case A =:= B of false -> Diff; true -> Same end.
            "#,
        )
    }

    #[test]
    fn does_not_break_macro_since_the_name_is_a_form_of_documentation() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         -define(IS_EQ(X,Y), X =:= Y).

         fn(A, Same, Diff) ->
            case ?IS_EQ(A, b) of false -> Diff; true -> Same end.
            "#,
        )
    }

    #[test]
    fn ignores_cases_where_neither_side_is_safe_as_a_pattern() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(Same, Diff) ->
            case erlang:monotonic_time() =:= #{} of true -> Same; _ -> Diff end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case #{} =:= erlang:monotonic_time() of true -> Same; _ -> Diff end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(Same, Diff) ->
            case (#{a := b}) =:= #{} of true -> Same; _ -> Diff end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(Same, Diff) ->
            case #{} =:= (#{a := b}) of true -> Same; _ -> Diff end.
            "#,
        )
    }

    #[test]
    fn detects_when_different_case_matches_on_wildcard() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =:= B of true -> Same; _ -> Diff end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn detects_when_just_lhs_is_not_a_valid_pattern() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_a() -> a.

         fn(Same, Diff) ->
            case get_a() =:= foo of true -> Same; _ -> Diff end.
         %%      ^^^^^^^^^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn detects_when_just_rhs_is_not_a_valid_pattern() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_b() -> b.

         fn(Same, Diff) ->
            case foo =:= get_b() of true -> Same; _ -> Diff end.
         %%      ^^^^^^^^^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn ignores_when_neither_lhs_nor_rhs_are_a_valid_pattern() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_a() -> a.
         get_b() -> b.

         fn(Same, Diff) ->
            case get_a() =:= get_b() of true -> Same; _ -> Diff end.
            "#,
        )
    }

    #[test]
    fn rewrites_equality_op_with_wildcard_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~:= b of true -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn rewrites_equality_op_false_with_wildcard_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~:= b of false -> Diff; _ -> Same end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn rewrites_even_if_wildcard_is_named() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =~:= B of false -> Diff; _True -> Same end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(B, Same, Diff) ->
            case B of a -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn rewrites_equality_op_with_false_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, b, Same, Diff) ->
            case A =~:= b of true -> Same; false -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, b, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn rewrites_equality_op_when_lhs_is_not_a_valid_pattern() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_a() -> a.

         fn(Same, Diff) ->
            case get_a() =~:= foo of true -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         get_a() -> a.

         fn(Same, Diff) ->
            case get_a() of foo -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn rewrites_equality_op_when_rhs_is_not_a_valid_pattern() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_b() -> b.

         fn(Same, Diff) ->
            case foo =~:= get_b() of true -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         get_b() -> b.

         fn(Same, Diff) ->
            case get_b() of foo -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn neq_detects_when_different_case_matches_on_false_and_true() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =/= b of false -> Same; true -> Diff end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =/= B of false -> Same; true -> Diff end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn neq_detects_when_different_case_matches_on_true_and_false() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =/= b of true -> Diff; false -> Same end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =/= B of true -> Diff; false -> Same end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn neq_detects_when_different_case_matches_on_wildcard() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =/= b of false -> Same; _ -> Diff end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =/= B of false -> Same; _ -> Diff end.
         %%      ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        );
    }

    #[test]
    fn neq_detects_when_just_lhs_is_not_a_valid_pattern() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_a() -> a.

         fn(Same, Diff) ->
            case get_a() =/= bar of false -> Same; _ -> Diff end.
         %%      ^^^^^^^^^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn neq_detects_when_just_rhs_is_not_a_valid_pattern() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_b() -> b.

         fn(Same, Diff) ->
            case bar =/= get_b() of false -> Same; _ -> Diff end.
         %%      ^^^^^^^^^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn neq_ignores_when_neither_lhs_nor_rhs_are_a_valid_pattern() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_a() -> a.
         get_b() -> b.

         fn(Same, Diff) ->
            case get_a() =/= get_b() of false -> Same; _ -> Diff end.
            "#,
        )
    }

    #[test]
    fn neq_rewrites_equality_op_with_wildcard_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~/= b of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =~/= B of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(B, Same, Diff) ->
            case B of a -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn neq_rewrites_equality_op_true_with_wildcard_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~/= b of true -> Diff; _ -> Same end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =~/= B of true -> Diff; _ -> Same end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(B, Same, Diff) ->
            case B of a -> Same; _ -> Diff end.
            "#]],
        );
    }

    #[test]
    fn neq_rewrites_equality_op_with_false_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~/= b of false -> Same; true -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn neq_rewrites_equality_op_when_lhs_is_not_a_valid_pattern() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_a() -> a.

         fn(Same, Diff) ->
            case get_a() =~/= bar of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         get_a() -> a.

         fn(Same, Diff) ->
            case get_a() of bar -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn neq_rewrites_equality_op_when_rhs_is_not_a_valid_pattern() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         get_b() -> b.

         fn(Same, Diff) ->
            case foo =~/= get_b() of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         get_b() -> b.

         fn(Same, Diff) ->
            case get_b() of foo -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn descriminee_favours_non_literal_list() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case [] =~/= A of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of [] -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~/= [] of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of [] -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn descriminee_favours_non_literal_tuple() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case {x,y} =~/= A of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of {x,y} -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~/= {x,y} of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of {x,y} -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn descriminee_favours_non_literal_empty_binary() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case <<>> =~/= A of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of <<>> -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~/= <<>> of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of <<>> -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn descriminee_favours_non_literal_non_empty_binary() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case <<"foo"/utf8>> =~/= A of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of <<"foo"/utf8>> -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~/= <<"foo"/utf8>> of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of <<"foo"/utf8>> -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn descriminee_favours_expression_with_more_bindings() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case <<"foo"/utf8>> =~/= <<A/binary, 1:1>> of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case <<A/binary, 1:1>> of <<"foo"/utf8>> -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case <<A/binary, 1:1>> =~/= <<"foo"/utf8>> of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case <<A/binary, 1:1>> of <<"foo"/utf8>> -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn descriminee_favours_less_nested_expression() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, B, Same, Diff) ->
            case {A,b} =~/= {{a,B},b} of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, B, Same, Diff) ->
            case {A,b} of {{a,B},b} -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, B, Same, Diff) ->
            case {{a,B},b} =~/= {A,b} of false -> Same; _ -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, B, Same, Diff) ->
            case {A,b} of {{a,B},b} -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn handled_precedence_correctly_by_not_matching_compound_expressions_which_happen_to_contain_equality_operator()
     {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, B) ->
            case A =:= -1 orelse B =:= -1 of
                false -> ok;
                _ -> -1
            end.
            "#,
        )
    }

    #[test]
    fn rejects_binaries_with_unsized_last_seg_as_patterns() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, B, Result) ->
                case <<"v1"/binary, A/binary>> =:= <<"v"/binary, B/binary>> of
                    true ->
                        {true, Result};
                    false ->
                        false
                end.
            "#,
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1"/binar~y, A/binary>> =:= <<"v1">> of
                    true ->
                        {true, Result};
                    false ->
                        false
                end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1"/binary, A/binary>> of <<"v1">> -> {true, Result}; _ -> false end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1">> =:= <<"v1"/bi~nary, A/binary>> of
                    true ->
                        {true, Result};
                    false ->
                        false
                end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1"/binary, A/binary>> of <<"v1">> -> {true, Result}; _ -> false end.
            "#]],
        )
    }

    #[test]
    fn applies_when_all_but_last_binary_seg_is_sized() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, B, Result) ->
                case <<"v1"/utf8, A/bina~ry>> =:= <<"v"/utf8, B/binary>> of
                    true ->
                        {true, Result};
                    false ->
                        false
                end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, B, Result) ->
                case <<"v1"/utf8, A/binary>> of <<"v"/utf8, B/binary>> -> {true, Result}; _ -> false end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1"/utf8, A/binar~y>> =:= <<"v1">> of
                    true ->
                        {true, Result};
                    false ->
                        false
                end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1"/utf8, A/binary>> of <<"v1">> -> {true, Result}; _ -> false end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1b">> =:= <<"v1"/u~tf8, A/binary>> of
                    true ->
                        {true, Result};
                    false ->
                        false
                end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Result) ->
                case <<"v1"/utf8, A/binary>> of <<"v1b">> -> {true, Result}; _ -> false end.
            "#]],
        )
    }

    #[test]
    fn eq_detects_if() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            if A =:= b -> Same; true -> Diff end.
         %%    ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            if a =:= B -> Same; true -> Diff end.
         %%    ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        );
    }

    #[test]
    fn neq_detects_if() {
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            if A =/= b -> Diff; true -> Same end.
         %%    ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            if a =/= B -> Diff; true -> Same end.
         %%    ^^^^^^^💡 information: Consider rewriting to an equality match.
            "#,
        )
    }

    #[test]
    fn eq_rewrites_if_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            if A =~:= b -> Same; true -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            if a =~:= B -> Same; true -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(B, Same, Diff) ->
            case B of a -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn neq_rewrites_if_to_equality_match() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            if A =/= b -> D~iff; true -> Same end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A of b -> Same; _ -> Diff end.
            "#]],
        );
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            if a =/= B -> D~iff; true -> Same end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(B, Same, Diff) ->
            case B of a -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn preserves_comments_on_case_expression() {
        check_fix(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            % My important comment
            case A =~:= b of true -> Same; false -> Diff end.
            "#,
            expect![[r#"
         -module(equality_op).

         fn(A, Same, Diff) ->
            % My important comment
            case A of b -> Same; _ -> Diff end.
            "#]],
        )
    }

    #[test]
    fn preserves_comments_on_patterns() {
        // For now, we don't fire the diagnostic if there are comment which might be
        // affected, but in future, we want to explicitly preserve comments
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(A, Same, Diff) ->
            case A =~:= b of
                % My important comment on same pattern
                true -> Same;
                % My important comment on different pattern
                false -> Diff
            end.
            "#,
        )
    }

    #[test]
    fn preserves_comments_on_branch_bodies() {
        // For now, we don't fire the diagnostic if there are comment which might be
        // affected, but in future, we want to explicitly preserve comments
        check_diagnostics(
            r#"
         //- /src/equality_op.erl
         -module(equality_op).

         fn(B, Same, Diff) ->
            case a =~:= B of
                true ->
                    % My important comment on same body
                    Same;
                false ->
                    % My important comment on different body
                    Diff
            end.
            "#,
        )
    }
}
