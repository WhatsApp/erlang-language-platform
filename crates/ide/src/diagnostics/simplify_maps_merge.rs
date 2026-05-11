/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: simplify_maps_merge
//!
//! Flag `maps:merge/2` calls that can be simplified:
//!
//!   * `maps:merge(M, #{})`                  → `M`                    (no-op merge)
//!   * `maps:merge(#{}, M)`                  → `M`                    (no-op merge)
//!   * `maps:merge(M, #{K => V, ...})`       → `M#{K => V, ...}`      (map update syntax)
//!   * `maps:merge(#{a => 1}, #{b => 2})`    → `#{a => 1, b => 2}`    (combined literal)
//!   * `maps:merge(M#{x => 1}, #{K => V})`   → `M#{x => 1, K => V}`   (extend existing update)
//!
//! In every form, the non-empty side is required to be a bound variable,
//! a map literal, or a map update expression — calls where it is a
//! different kind of expression (function calls, macro calls,
//! comprehensions, etc.) are not flagged because the rewrite would not
//! be a syntactic substitution.
//!
//! When the first argument is itself a non-empty map literal or a map
//! update, the rewrite appends the second literal's fields to the
//! existing field list rather than emitting `Lit1#{Lit2_fields}`. For
//! map literals this avoids triggering `L1318` ("expression updates a
//! literal"); for map updates it avoids a redundant nested update.
//! Erlang's `=>` keeps the rightmost value on duplicate keys, matching
//! `maps:merge/2`'s "second argument wins" behaviour.

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::PlaceholderMatch;
use elp_ide_ssr::SubId;
use elp_ide_ssr::is_placeholder_a_var_from_sema_and_match;
use hir::AnyExprId;
use hir::Expr;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::Category;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

static MAP_VAR: &str = "_@Map";
static MAP_LITERAL_VAR: &str = "_@MapLiteral";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum MergeContext {
    /// `maps:merge(_, #{})` — second argument is the empty map literal.
    EmptySecondArg,
    /// `maps:merge(#{}, _)` — first argument is the empty map literal.
    EmptyFirstArg,
    /// `maps:merge(_, #{...})` — second argument is a non-empty map literal.
    UpdateSyntax,
}

pub(crate) struct SimplifyMapsMergeLinter;

impl Linter for SimplifyMapsMergeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::SimplifyMapsMerge
    }

    fn description(&self) -> &'static str {
        "Consider using map update syntax instead"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for SimplifyMapsMergeLinter {
    type Context = MergeContext;

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, MergeContext)> = vec![
                (
                    format!("ssr: maps:merge({MAP_VAR}, #{{}})."),
                    MergeContext::EmptySecondArg,
                ),
                (
                    format!("ssr: maps:merge(#{{}}, {MAP_VAR})."),
                    MergeContext::EmptyFirstArg,
                ),
                (
                    format!("ssr: maps:merge({MAP_VAR}, {MAP_LITERAL_VAR})."),
                    MergeContext::UpdateSyntax,
                ),
            ];
        }
        &PATTERNS
    }

    fn pattern_description(&self, context: &Self::Context) -> &'static str {
        match context {
            MergeContext::EmptySecondArg | MergeContext::EmptyFirstArg => {
                "Merging with an empty map is a no-op"
            }
            MergeContext::UpdateSyntax => "Consider using map update syntax instead",
        }
    }

    fn is_match_valid(
        &self,
        context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<bool> {
        let body_arc = matched.matched_node_body.get_body(ctx.sema)?;
        let body = body_arc.as_ref();
        let kind = |m: &PlaceholderMatch| -> Option<&Expr> {
            match m.code_id {
                SubId::AnyExprId(AnyExprId::Expr(expr_id)) => Some(&body.exprs[expr_id]),
                _ => None,
            }
        };
        let is_map_literal = |m: &PlaceholderMatch| matches!(kind(m), Some(Expr::Map { .. }));
        let is_empty_map_literal = |m: &PlaceholderMatch| matches!(kind(m), Some(Expr::Map { fields }) if fields.is_empty());
        let is_non_empty_map_literal = |m: &PlaceholderMatch| matches!(kind(m), Some(Expr::Map { fields }) if !fields.is_empty());
        let is_map_update = |m: &PlaceholderMatch| matches!(kind(m), Some(Expr::MapUpdate { .. }));
        let is_extendable = |m: &PlaceholderMatch| {
            is_placeholder_a_var_from_sema_and_match(ctx.sema, matched, m)
                || is_map_literal(m)
                || is_map_update(m)
        };

        match context {
            MergeContext::EmptySecondArg => {
                let arg = matched.get_placeholder_match(MAP_VAR)?;
                Some(is_extendable(&arg))
            }
            MergeContext::EmptyFirstArg => {
                let arg = matched.get_placeholder_match(MAP_VAR)?;
                // `maps:merge(#{}, #{})` is already covered by EmptySecondArg.
                if is_empty_map_literal(&arg) {
                    return Some(false);
                }
                Some(is_extendable(&arg))
            }
            MergeContext::UpdateSyntax => {
                let arg1 = matched.get_placeholder_match(MAP_VAR)?;
                let arg2 = matched.get_placeholder_match(MAP_LITERAL_VAR)?;
                // Empty cases are covered by EmptySecondArg / EmptyFirstArg.
                if !is_non_empty_map_literal(&arg2) {
                    return Some(false);
                }
                if is_empty_map_literal(&arg1) {
                    return Some(false);
                }
                Some(is_extendable(&arg1))
            }
        }
    }

    fn fixes(
        &self,
        context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let range = matched.range.range;
        let mut builder = SourceChangeBuilder::new(ctx.file_id);
        let (replacement, label) = match context {
            MergeContext::EmptySecondArg | MergeContext::EmptyFirstArg => {
                let arg = matched.placeholder_text(ctx.sema, MAP_VAR)?;
                (arg, "Drop redundant merge with empty map")
            }
            MergeContext::UpdateSyntax => {
                let map = matched.placeholder_text(ctx.sema, MAP_VAR)?;
                let map_literal = matched.placeholder_text(ctx.sema, MAP_LITERAL_VAR)?;
                if !map_literal.starts_with('#') {
                    return None;
                }
                // When the first argument is itself a map literal or a map
                // update, append the second literal's fields to the existing
                // field list rather than emit `Lit1#{Lit2_fields}` — the
                // latter triggers L1318 ("expression updates a literal") for
                // map literals, and produces a redundant nested update for
                // `M#{x => 1}`. Erlang's `=>` keeps the rightmost value on
                // duplicate keys, preserving `maps:merge/2`'s "second
                // argument wins" semantics.
                let body_arc = matched.matched_node_body.get_body(ctx.sema)?;
                let body = body_arc.as_ref();
                let arg1 = matched.get_placeholder_match(MAP_VAR)?;
                let arg1_extends_in_place = match arg1.code_id {
                    SubId::AnyExprId(AnyExprId::Expr(expr_id)) => {
                        matches!(
                            &body.exprs[expr_id],
                            Expr::Map { fields } if !fields.is_empty()
                        ) || matches!(&body.exprs[expr_id], Expr::MapUpdate { .. })
                    }
                    _ => false,
                };
                let replacement = if arg1_extends_in_place {
                    let map_no_close = map.strip_suffix('}')?;
                    let lit_inner = map_literal
                        .strip_prefix("#{")
                        .and_then(|s| s.strip_suffix('}'))?;
                    let separator = if lit_inner.starts_with(char::is_whitespace) {
                        ","
                    } else {
                        ", "
                    };
                    format!("{map_no_close}{separator}{lit_inner}}}")
                } else {
                    format!("{map}{map_literal}")
                };
                (replacement, "Rewrite to use map update syntax")
            }
        };
        builder.replace(range, replacement);
        Some(vec![fix(
            "simplify_maps_merge",
            label,
            builder.finish(),
            range,
        )])
    }

    fn add_categories(&self, _context: &Self::Context) -> Vec<Category> {
        vec![Category::SimplificationRule]
    }
}

pub(crate) static LINTER: SimplifyMapsMergeLinter = SimplifyMapsMergeLinter;

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::SimplifyMapsMerge
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::RedundantFunWrapper);
        tests::check_fix_with_config(config, fixture_before, fixture_after)
    }

    // ============================================================
    // Negative cases — diagnostic must NOT fire
    // ============================================================

    #[test]
    fn ignores_when_first_arg_is_not_var_or_literal() {
        // Function call as first arg.
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         get_map() -> #{}.

         % elp:ignore W0017 (undefined_function)
         fn(K, V) -> maps:merge(get_map(), #{K => V}).
            "#,
        )
    }

    #[test]
    fn ignores_when_first_arg_is_record_field_access() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         -record(state, {map}).

         % elp:ignore W0017 (undefined_function)
         fn(S, K, V) -> maps:merge(S#state.map, #{K => V}).
            "#,
        )
    }

    #[test]
    fn detects_when_first_arg_is_map_update() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(M, K, V) -> maps:merge(M#{x => 1}, #{K => V}).
         %%             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0080: Consider using map update syntax instead
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_not_a_literal_var() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(M1, M2) -> maps:merge(M1, M2).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_function_call() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(M, K, V) -> maps:merge(M, maps:put(K, V, #{})).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_map_update() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(M1, M2) -> maps:merge(M1, M2#{a => 1}).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_map_comprehension() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map, Pairs) -> maps:merge(Map, #{K => V || {K, V} <- Pairs}).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_case_expression() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map, X) ->
            maps:merge(Map, case X of
                ok -> #{a => 1};
                _ -> #{b => 2}
            end).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_begin_block() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> maps:merge(Map, begin #{a => 1} end).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_macro() {
        // Pattern explicitly excludes macros ("but not in macros").
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         -define(EXTRAS, #{a => 1}).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> maps:merge(Map, ?EXTRAS).
            "#,
        )
    }

    #[test]
    fn ignores_when_first_arg_is_macro() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         -define(BASE, #{a => 1}).

         % elp:ignore W0017 (undefined_function)
         fn() -> maps:merge(?BASE, #{b => 2}).
            "#,
        )
    }

    #[test]
    fn ignores_when_both_args_are_macros() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         -define(BASE, #{a => 1}).
         -define(EXTRAS, #{b => 2}).

         % elp:ignore W0017 (undefined_function)
         fn() -> maps:merge(?BASE, ?EXTRAS).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_if_expression() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map, Flag) ->
            maps:merge(Map, if Flag -> #{a => 1}; true -> #{b => 2} end).
            "#,
        )
    }

    #[test]
    fn ignores_when_second_arg_is_try_expression() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) ->
            maps:merge(Map, try #{a => 1} catch _:_ -> #{} end).
            "#,
        )
    }

    #[test]
    fn ignores_call_inside_macro_body() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         -define(MAPS_MERGE(M, K, V), maps:merge(M, #{K => V})).

         % elp:ignore W0017 (undefined_function)
         fn(M, K, V) -> ?MAPS_MERGE(M, K, V).
            "#,
        )
    }

    #[test]
    fn ignores_wrong_arity_maps_merge_with() {
        // maps:merge_with/3 — different function, must not match.
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(M, F) -> maps:merge_with(F, M, #{a => 1}).
            "#,
        )
    }

    #[test]
    fn ignores_empty_first_when_second_is_function_call() {
        // We refuse to rewrite `maps:merge(#{}, F())` to `F()` because
        // `F()` may have side effects we do not want to commit to.
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         get_map() -> #{a => 1}.

         % elp:ignore W0017 (undefined_function)
         fn() -> maps:merge(#{}, get_map()).
            "#,
        )
    }

    #[test]
    fn ignores_empty_second_when_first_is_function_call() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         get_map() -> #{a => 1}.

         % elp:ignore W0017 (undefined_function)
         fn() -> maps:merge(get_map(), #{}).
            "#,
        )
    }

    // ============================================================
    // Positive cases — detection annotations
    // ============================================================

    #[test]
    fn detects_update_syntax_form() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:merge(Map, #{K => V}).
         %%               ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0080: Consider using map update syntax instead
            "#,
        )
    }

    #[test]
    fn detects_empty_second_arg() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> maps:merge(Map, #{}).
         %%         ^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0080: Merging with an empty map is a no-op
            "#,
        )
    }

    #[test]
    fn detects_empty_first_arg() {
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> maps:merge(#{}, Map).
         %%         ^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0080: Merging with an empty map is a no-op
            "#,
        )
    }

    #[test]
    fn detects_only_once_when_both_args_empty() {
        // Two patterns could match `maps:merge(#{}, #{})`, but EmptyFirstArg
        // is filtered to avoid duplicate diagnostics.
        check_diagnostics(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn() -> maps:merge(#{}, #{}).
         %%      ^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0080: Merging with an empty map is a no-op
            "#,
        )
    }

    // ============================================================
    // Empty merge: rewrite tests
    // ============================================================

    #[test]
    fn rewrites_empty_second_arg_with_var() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> maps:m~erge(Map, #{}).
            "#,
            expect![[r#"
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> Map.
            "#]],
        )
    }

    #[test]
    fn rewrites_empty_first_arg_with_var() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> maps:m~erge(#{}, Map).
            "#,
            expect![[r#"
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> Map.
            "#]],
        )
    }

    #[test]
    fn rewrites_empty_second_arg_with_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(#{a => 1, b => 2}, #{}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            #{a => 1, b => 2}.
            "#]],
        )
    }

    #[test]
    fn rewrites_empty_first_arg_with_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(#{}, #{a => 1, b => 2}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            #{a => 1, b => 2}.
            "#]],
        )
    }

    #[test]
    fn rewrites_both_empty_to_empty() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(#{}, #{}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            #{}.
            "#]],
        )
    }

    #[test]
    fn rewrites_empty_second_arg_when_in_assignment() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Map) ->
            % elp:ignore W0017 (undefined_function)
            Result = maps:m~erge(Map, #{}),
            Result.
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Map) ->
            % elp:ignore W0017 (undefined_function)
            Result = Map,
            Result.
            "#]],
        )
    }

    // ============================================================
    // UpdateSyntax: rewrite tests
    // ============================================================

    #[test]
    fn rewrites_var_and_simple_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:m~erge(Map, #{K => V}).
            "#,
            expect![[r#"
         -module(maps_merge).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> Map#{K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_var_and_multiline_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, K1, V1, K2, V2, K3, V3) ->
            % elp:ignore W0017 (undefined_function)
            M1 = maps:m~erge(M, #{
                K1 => V1,
                K2 => V2,
                K3 => V3
            }),
            M1.
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, K1, V1, K2, V2, K3, V3) ->
            % elp:ignore W0017 (undefined_function)
            M1 = M#{
                K1 => V1,
                K2 => V2,
                K3 => V3
            },
            M1.
            "#]],
        )
    }

    #[test]
    fn rewrites_var_and_two_field_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, A, B) ->
            % elp:ignore W0017 (undefined_function)
            M1 = maps:m~erge(M, #{foo => A, bar => B}),
            M1.
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, A, B) ->
            % elp:ignore W0017 (undefined_function)
            M1 = M#{foo => A, bar => B},
            M1.
            "#]],
        )
    }

    #[test]
    fn rewrites_var_and_literal_in_call_arg() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            foo:bar(baz, maps:m~erge(M, #{K => V}), quux).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            foo:bar(baz, M#{K => V}, quux).
            "#]],
        )
    }

    #[test]
    fn rewrites_var_inside_list_comprehension_value() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, Items) ->
            #{
                % elp:ignore W0017 (undefined_function)
                {foo, Item} => maps:m~erge(M, #{
                    key => Item,
                    tag => foo
                })
             || Item <- Items
            }.
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, Items) ->
            #{
                % elp:ignore W0017 (undefined_function)
                {foo, Item} => M#{
                    key => Item,
                    tag => foo
                }
             || Item <- Items
            }.
            "#]],
        )
    }

    #[test]
    fn rewrites_var_and_literal_as_function_arg() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            foo(maps:m~erge(M, #{K => V}), quux).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            foo(M#{K => V}, quux).
            "#]],
        )
    }

    #[test]
    fn rewrites_var_and_large_multiline_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, A, B, C) ->
            % elp:ignore W0017 (undefined_function)
            M1 = maps:m~erge(M, #{
                foo => A,
                bar => B,
                baz => C,
                quux => 1000000,
                alpha => true,
                beta => false,
                gamma => ok,
                delta => []
            }),
            M1.
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, A, B, C) ->
            % elp:ignore W0017 (undefined_function)
            M1 = M#{
                foo => A,
                bar => B,
                baz => C,
                quux => 1000000,
                alpha => true,
                beta => false,
                gamma => ok,
                delta => []
            },
            M1.
            "#]],
        )
    }

    #[test]
    fn rewrites_var_inside_record_field() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         -record(state, {field}).

         fn(M, K1, K2, K3, K4) ->
            % elp:ignore W0017 (undefined_function)
            #state{
                field = maps:m~erge(M, #{
                    K1 => foo,
                    K2 => bar,
                    K3 => baz,
                    K4 => quux
                })
            }.
            "#,
            expect![[r#"
         -module(maps_merge).

         -record(state, {field}).

         fn(M, K1, K2, K3, K4) ->
            % elp:ignore W0017 (undefined_function)
            #state{
                field = M#{
                    K1 => foo,
                    K2 => bar,
                    K3 => baz,
                    K4 => quux
                }
            }.
            "#]],
        )
    }

    #[test]
    fn rewrites_inner_when_outer_is_not_eligible() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Outer, M, A, B, C) ->
            % elp:ignore W0017 (undefined_function)
            maps:merge(
                Outer,
                % elp:ignore W0017 (undefined_function)
                maps:m~erge(M, #{
                    foo => A,
                    bar => B,
                    baz => C
                })
            ).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Outer, M, A, B, C) ->
            % elp:ignore W0017 (undefined_function)
            maps:merge(
                Outer,
                % elp:ignore W0017 (undefined_function)
                M#{
                    foo => A,
                    bar => B,
                    baz => C
                }
            ).
            "#]],
        )
    }

    #[test]
    fn rewrites_var_when_map_is_a_local_binding() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(K, V) ->
            Map = #{a => b},
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(Map, #{K => V}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(K, V) ->
            Map = #{a => b},
            % elp:ignore W0017 (undefined_function)
            Map#{K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_var_in_lambda() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Maps) ->
            lists:map(
                % elp:ignore W0017 (undefined_function)
                fun(MapAcc) -> maps:m~erge(MapAcc, #{added => true}) end,
                Maps
            ).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Maps) ->
            lists:map(
                % elp:ignore W0017 (undefined_function)
                fun(MapAcc) -> MapAcc#{added => true} end,
                Maps
            ).
            "#]],
        )
    }

    #[test]
    fn rewrites_with_nested_map_value() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Map, Inner) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(Map, #{nested => #{a => 1, b => 2}, other => Inner}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Map, Inner) ->
            % elp:ignore W0017 (undefined_function)
            Map#{nested => #{a => 1, b => 2}, other => Inner}.
            "#]],
        )
    }

    #[test]
    fn rewrites_with_macro_in_value() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         -define(DEFAULT, default).

         fn(Map) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(Map, #{key => ?DEFAULT}).
            "#,
            expect![[r#"
         -module(maps_merge).

         -define(DEFAULT, default).

         fn(Map) ->
            % elp:ignore W0017 (undefined_function)
            Map#{key => ?DEFAULT}.
            "#]],
        )
    }

    #[test]
    fn rewrites_with_complex_value_expressions() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Map, X) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(Map, #{
                computed => 1 + 2 * X,
                concat => [a, b, c] ++ [d],
                tuple => {ok, X}
            }).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Map, X) ->
            % elp:ignore W0017 (undefined_function)
            Map#{
                computed => 1 + 2 * X,
                concat => [a, b, c] ++ [d],
                tuple => {ok, X}
            }.
            "#]],
        )
    }

    #[test]
    fn rewrites_with_arithmetic_value() {
        // Non-literal value expressions are allowed in the second map literal.
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(X, Y) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(X, #{a => Y + 1}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(X, Y) ->
            % elp:ignore W0017 (undefined_function)
            X#{a => Y + 1}.
            "#]],
        )
    }

    #[test]
    fn rewrites_with_function_call_value() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Map, List) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(Map, #{count => length(List), head => hd(List)}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Map, List) ->
            % elp:ignore W0017 (undefined_function)
            Map#{count => length(List), head => hd(List)}.
            "#]],
        )
    }

    #[test]
    fn rewrites_with_variable_keys_and_values() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Map, K1, K2, V1, V2) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(Map, #{K1 => V1, K2 => V2}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Map, K1, K2, V1, V2) ->
            % elp:ignore W0017 (undefined_function)
            Map#{K1 => V1, K2 => V2}.
            "#]],
        )
    }

    #[test]
    fn rewrites_assignment_with_following_statement() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(Map, K, V) ->
            % elp:ignore W0017 (undefined_function)
            Map2 = maps:m~erge(Map, #{K => V}),
            {ok, Map2}.
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(Map, K, V) ->
            % elp:ignore W0017 (undefined_function)
            Map2 = Map#{K => V},
            {ok, Map2}.
            "#]],
        )
    }

    // ============================================================
    // Two-literal cases — semantics must match maps:merge/2,
    // including "second wins" on overlapping keys.
    // ============================================================

    #[test]
    fn rewrites_two_literals_no_overlap() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(#{a => 1, b => 2}, #{c => 3, d => 4}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            #{a => 1, b => 2, c => 3, d => 4}.
            "#]],
        )
    }

    #[test]
    fn rewrites_two_literals_with_overlap_second_wins() {
        // For `maps:merge/2`, the second argument's bindings win on key
        // overlap. The rewrite concatenates both literals' fields into a
        // single map; Erlang's `=>` keeps the rightmost value on duplicate
        // keys, preserving the same observable behaviour.
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(#{a => 1, b => 2}, #{a => 3, c => 4}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            #{a => 1, b => 2, a => 3, c => 4}.
            "#]],
        )
    }

    #[test]
    fn rewrites_two_literals_with_full_overlap() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(#{a => 1, b => 2}, #{a => 3, b => 4}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn() ->
            % elp:ignore W0017 (undefined_function)
            #{a => 1, b => 2, a => 3, b => 4}.
            "#]],
        )
    }

    #[test]
    fn rewrites_two_literals_with_runtime_keys() {
        // Runtime keys can overlap unpredictably; concatenating the field
        // lists preserves `maps:merge/2`'s "second arg wins" semantics
        // because Erlang's `=>` keeps the rightmost value on duplicates.
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(K1, V1, K2, V2) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(#{K1 => V1}, #{K2 => V2}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(K1, V1, K2, V2) ->
            % elp:ignore W0017 (undefined_function)
            #{K1 => V1, K2 => V2}.
            "#]],
        )
    }

    // ============================================================
    // Map-update first arg — appended fields preserve "second wins"
    // semantics via Erlang's `=>` last-write-wins on duplicate keys.
    // ============================================================

    #[test]
    fn rewrites_map_update_and_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(M#{x => 1}, #{K => V}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            M#{x => 1, K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_update_with_overlapping_literal_key() {
        // The literal's `x` appears after the existing `x => 1`, so
        // Erlang's `=>` keeps the rightmost value, matching maps:merge/2.
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(M#{x => 1, y => 2}, #{x => 3, z => 4}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            M#{x => 1, y => 2, x => 3, z => 4}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_update_with_exact_op() {
        // `:=` requires `x` to exist in M; appending more fields with
        // `=>` preserves that requirement and final value.
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(M#{x := 1}, #{K => V}).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, K, V) ->
            % elp:ignore W0017 (undefined_function)
            M#{x := 1, K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_update_multiline_literal() {
        check_fix(
            r#"
         //- /src/maps_merge.erl
         -module(maps_merge).

         fn(M, A, B, C) ->
            % elp:ignore W0017 (undefined_function)
            maps:m~erge(M#{base => true}, #{
                a => A,
                b => B,
                c => C
            }).
            "#,
            expect![[r#"
         -module(maps_merge).

         fn(M, A, B, C) ->
            % elp:ignore W0017 (undefined_function)
            M#{base => true,
                a => A,
                b => B,
                c => C
            }.
            "#]],
        )
    }
}
