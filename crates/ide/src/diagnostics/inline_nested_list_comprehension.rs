/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: inline_nested_list_comprehension
//!
//! Detects nested list comprehensions of the form:
//!   `[f(X) || X <- [g(Y) || Y <- List]]`
//! and suggests inlining them to:
//!   `[f(g(Y)) || Y <- List]`
//!
//! The transformation is only valid when the outer variable X appears
//! exactly once in the body expression, to avoid duplicating computation.

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::BodyOrigin;
use hir::Expr;
use hir::FormIdx;
use hir::InFile;
use hir::Semantic;
use hir::fold::FoldCtx;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::Category;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

static BODY_VAR: &str = "_@Body";
static OUTER_VAR: &str = "_@X";
static INNER_EXPR_VAR: &str = "_@InnerExpr";
static INNER_VAR: &str = "_@Y";
static LIST_VAR: &str = "_@List";

pub(crate) struct InlineNestedListComprehensionLinter;

impl Linter for InlineNestedListComprehensionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::InlineNestedListComprehension
    }

    fn description(&self) -> &'static str {
        "Nested list comprehension can be inlined to avoid intermediate list allocation."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for InlineNestedListComprehensionLinter {
    type Context = ();

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> = vec![(
                format!(
                    "ssr: [{BODY_VAR} || {OUTER_VAR} <- [{INNER_EXPR_VAR} || {INNER_VAR} <- {LIST_VAR}]]."
                ),
                (),
            ),];
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

        let _outer_var = matched.placeholder_is_var(sema, OUTER_VAR)?;

        let occurrence_count = count_var_occurrences_in_body(sema, matched)?;
        if occurrence_count != 1 {
            return None;
        }

        Some(true)
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let inner_expr_text = matched.placeholder_text(sema, INNER_EXPR_VAR)?;
        let inner_var_text = matched.placeholder_text(sema, INNER_VAR)?;
        let list_text = matched.placeholder_text(sema, LIST_VAR)?;
        let body_text = matched.placeholder_text(sema, BODY_VAR)?;
        let body_range = matched.placeholder_range(sema, BODY_VAR)?;

        let var_range = find_single_var_occurrence_in_body(sema, matched)?;

        let new_body =
            substitute_var_in_body(&body_text, body_range.start(), &var_range, &inner_expr_text)?;

        let mut builder = SourceChangeBuilder::new(file_id);
        let inlined = format!("[{new_body} || {inner_var_text} <- {list_text}]");
        builder.replace(matched.range.range, inlined);

        let fixes = vec![fix(
            "inline_nested_list_comprehension",
            "Inline nested list comprehension",
            builder.finish(),
            matched.range.range,
        )];
        Some(fixes)
    }

    fn add_categories(&self, _context: &Self::Context) -> Vec<Category> {
        vec![Category::SimplificationRule]
    }
}

pub(crate) static LINTER: InlineNestedListComprehensionLinter = InlineNestedListComprehensionLinter;

/// Helper to get an `InFunctionClauseBody` from a `BodyOrigin`.
fn get_in_function_clause_body<'a>(
    sema: &'a Semantic<'a>,
    body_origin: &BodyOrigin,
) -> Option<hir::InFunctionClauseBody<'a, ()>> {
    match body_origin {
        BodyOrigin::FormIdx {
            file_id,
            form_id: FormIdx::FunctionClause(function_clause_id),
        } => Some(sema.to_function_clause_body(InFile::new(*file_id, *function_clause_id))),
        _ => None,
    }
}

/// Count occurrences of the outer variable within the body expression.
/// Uses HIR traversal with scope-aware variable resolution to correctly handle
/// variable shadowing in nested scopes (e.g., lambda parameters).
fn count_var_occurrences_in_body(sema: &Semantic, matched: &Match) -> Option<usize> {
    let outer_var_match = matched.get_placeholder_match(sema, OUTER_VAR)?;
    let outer_var_def = get_var_def_from_match(sema, matched, &outer_var_match)?;

    let body_match = matched.get_placeholder_match(sema, BODY_VAR)?;
    let body = matched.matched_node_body.get_body(sema)?;
    let any_expr_id = body_match.code_id.any_expr_id()?;
    let expr_id = match any_expr_id {
        AnyExprId::Expr(id) => id,
        _ => return None,
    };

    let in_clause = get_in_function_clause_body(sema, &matched.matched_node_body)?;

    let strategy = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::InvisibleParens,
    };

    let count = FoldCtx::fold_expr(
        strategy,
        body.as_ref(),
        expr_id,
        0,
        &mut |acc, ctx| match ctx.item {
            AnyExpr::Expr(Expr::Var(_)) => {
                if let AnyExprId::Expr(var_expr_id) = ctx.item_id
                    && let Some(hir::DefinitionOrReference::Reference(ref_defs)) =
                        in_clause.to_var_def_expr(var_expr_id)
                    && ref_defs.iter().any(|d| d == &outer_var_def)
                {
                    return acc + 1;
                }
                acc
            }
            _ => acc,
        },
    );

    Some(count)
}

/// Get the VarDef for a variable from a placeholder match.
fn get_var_def_from_match(
    sema: &Semantic,
    matched: &Match,
    placeholder_match: &elp_ide_ssr::PlaceholderMatch,
) -> Option<hir::VarDef> {
    let in_clause = get_in_function_clause_body(sema, &matched.matched_node_body)?;
    let any_expr_id = placeholder_match.code_id.any_expr_id()?;
    match in_clause.to_var_def_any(any_expr_id)? {
        hir::DefinitionOrReference::Definition(def) => Some(def),
        hir::DefinitionOrReference::Reference(mut refs) if refs.len() == 1 => {
            Some(refs.swap_remove(0))
        }
        _ => None,
    }
}

/// Find the source range of the single variable occurrence in the body expression.
/// Uses HIR traversal with scope-aware variable resolution.
fn find_single_var_occurrence_in_body(sema: &Semantic, matched: &Match) -> Option<TextRange> {
    let outer_var_match = matched.get_placeholder_match(sema, OUTER_VAR)?;
    let outer_var_def = get_var_def_from_match(sema, matched, &outer_var_match)?;

    let body_match = matched.get_placeholder_match(sema, BODY_VAR)?;
    let body = matched.matched_node_body.get_body(sema)?;
    let any_expr_id = body_match.code_id.any_expr_id()?;
    let expr_id = match any_expr_id {
        AnyExprId::Expr(id) => id,
        _ => return None,
    };

    let in_clause = get_in_function_clause_body(sema, &matched.matched_node_body)?;

    let strategy = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::InvisibleParens,
    };

    FoldCtx::fold_expr(
        strategy,
        body.as_ref(),
        expr_id,
        None,
        &mut |acc, ctx| match ctx.item {
            AnyExpr::Expr(Expr::Var(_)) => {
                if let AnyExprId::Expr(var_expr_id) = ctx.item_id
                    && let Some(hir::DefinitionOrReference::Reference(ref_defs)) =
                        in_clause.to_var_def_expr(var_expr_id)
                    && ref_defs.iter().any(|d| d == &outer_var_def)
                {
                    return ctx
                        .find_range(sema)
                        .map(|(_body, file_range)| file_range.range)
                        .or(acc);
                }
                acc
            }
            _ => acc,
        },
    )
}

/// Replace the variable occurrence in the body text with the inner expression.
/// Uses absolute source ranges to compute relative offsets within the body text.
fn substitute_var_in_body(
    body_text: &str,
    body_start: TextSize,
    var_range: &TextRange,
    replacement: &str,
) -> Option<String> {
    let rel_start = u32::from(var_range.start()) - u32::from(body_start);
    let rel_end = u32::from(var_range.end()) - u32::from(body_start);

    let start_idx = rel_start as usize;
    let end_idx = rel_end as usize;

    if start_idx <= body_text.len() && end_idx <= body_text.len() && start_idx <= end_idx {
        let mut result = body_text.to_string();
        result.replace_range(start_idx..end_idx, replacement);
        Some(result)
    } else {
        None
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
        d.code == DiagnosticCode::InlineNestedListComprehension
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    // ==================== POSITIVE TESTS (should trigger) ====================

    #[test]
    fn test_simple_inline() {
        check_diagnostics(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [f(X) || X <- [g(Y) || Y <- List]].
         %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0070: Nested list comprehension can be inlined to avoid intermediate list allocation.
            "#,
        );
    }

    #[test]
    fn test_fix_simple() {
        check_fix(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [f(X) || X ~<- [g(Y) || Y <- List]].
            "#,
            expect![[r#"
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [f(g(Y)) || Y <- List].
            "#]],
        );
    }

    #[test]
    fn test_identity_inner_expr() {
        check_fix(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [f(X) || X ~<- [Y || Y <- List]].
            "#,
            expect![[r#"
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [f(Y) || Y <- List].
            "#]],
        );
    }

    #[test]
    fn test_var_in_nested_call() {
        check_fix(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [outer(inner(X)) || X ~<- [g(Y) || Y <- List]].
            "#,
            expect![[r#"
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [outer(inner(g(Y))) || Y <- List].
            "#]],
        );
    }

    // ==================== NEGATIVE TESTS (should NOT trigger) ====================

    #[test]
    fn test_no_trigger_multiple_occurrences() {
        check_diagnostics(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [{X, X + 1} || X <- [g(Y) || Y <- List]].
            "#,
        );
    }

    #[test]
    fn test_no_trigger_three_occurrences() {
        check_diagnostics(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [{X, X + 1, X * 2} || X <- [g(Y) || Y <- List]].
            "#,
        );
    }

    #[test]
    fn test_no_trigger_var_unused() {
        check_diagnostics(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [42 || X <- [g(Y) || Y <- List]].
            "#,
        );
    }

    // ==================== EDGE CASES ====================

    #[test]
    fn test_triply_nested_comprehension() {
        // The outermost nesting should trigger. After fixing, the result
        // still contains a nested comprehension that would trigger again
        // on a second pass.
        check_fix(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [f(X) || X ~<- [g(Y) || Y <- [h(Z) || Z <- List]]].
            "#,
            expect![[r#"
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [f(g(Y)) || Y <- [h(Z) || Z <- List]].
            "#]],
        );
    }

    #[test]
    fn test_var_not_substring() {
        check_fix(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [h(X) || X ~<- [g(Y) || Y <- List]].
            "#,
            expect![[r#"
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             [h(g(Y)) || Y <- List].
            "#]],
        );
    }

    #[test]
    fn test_subscopes_are_correctly_not_counted_as_usages() {
        check_fix(
            r#"
         //- /src/test.erl
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             % The `X` in the lambda here exists in a nested scope, so it should
             % not prevent the inlining of the outer `X`.
             [h(X, fun (X) -> X end) || X ~<- [g(Y) || Y <- List]].
            "#,
            expect![[r#"
         -module(test).
         % elp:ignore W0017 (undefined_function)
         foo(List) ->
             % The `X` in the lambda here exists in a nested scope, so it should
             % not prevent the inlining of the outer `X`.
             [h(g(Y), fun (X) -> X end) || Y <- List].
            "#]],
        );
    }
}
