/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: ets_lookup_to_lookup_element
//!
//! Detect patterns where `ets:lookup/2` is used in a `case` expression
//! that immediately destructures the result to extract a value or return
//! a default, and suggest using `ets:lookup_element/4` instead.
//!
//! e.g.
//!
//! ```ignore
//! case ets:lookup(Table, Key) of
//!     [{_, V}] -> V;
//!     _ -> undefined
//! end
//! ```
//!
//! becomes
//!
//! ```ignore
//! ets:lookup_element(Table, Key, 2, undefined).
//! ```
//!
//! The matched tuple key (first element) can be a wildcard (`_`),
//! an underscore-prefixed variable (`_K`), or even the lookup key
//! itself (`Key`), since `ets:lookup` already filters by key.

use elp_ide_assists::Assist;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::PlaceholderMatch;
use elp_ide_ssr::SubId;
use hir::AnyExprId;
use hir::Body;
use hir::Expr;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use lazy_static::lazy_static;

use crate::codemod_helpers::is_wildcard;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct EtsLookupToLookupElementLinter;

impl Linter for EtsLookupToLookupElementLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::EtsLookupToLookupElement
    }

    fn description(&self) -> &'static str {
        "Can be rewritten using `ets:lookup_element/4` for improved performance."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for EtsLookupToLookupElementLinter {
    type Context = ();

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> = vec![
                (
                    format!(
                        "ssr: case ets:lookup({TAB_VAR},{KEY_VAR}) of
                            [{{{RESULT_KEY_VAR}, {VAL_VAR}}}] -> {VAL_VAR};
                            [] -> {DEFAULT_VAR}
                          end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case ets:lookup({TAB_VAR},{KEY_VAR}) of
                            [] -> {DEFAULT_VAR};
                            [{{{RESULT_KEY_VAR}, {VAL_VAR}}}] -> {VAL_VAR}
                          end."
                    ),
                    (),
                ),
                (
                    format!(
                        "ssr: case ets:lookup({TAB_VAR},{KEY_VAR}) of
                            [{{{RESULT_KEY_VAR}, {VAL_VAR}}}] -> {VAL_VAR};
                            _ -> {DEFAULT_VAR}
                          end."
                    ),
                    (),
                ),
            ];
        }
        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<bool> {
        if let Some(comments) = matched.comments(sema)
            && !comments.is_empty()
        {
            return None;
        }
        let default_match = matched.get_placeholder_match(sema, DEFAULT_VAR)?;
        let body_arc = matched.matched_node_body.get_body(sema)?;
        let body = body_arc.as_ref();
        if !is_not_a_computation(body, &default_match) {
            return None;
        }
        let key_text = matched.placeholder_text(sema, KEY_VAR)?;
        let result_key_text = matched.placeholder_text(sema, RESULT_KEY_VAR)?;
        if key_text == result_key_text
            || is_wildcard_match(
                sema,
                body,
                &matched.get_placeholder_match(sema, RESULT_KEY_VAR),
            )
        {
            Some(true)
        } else {
            None
        }
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let tab = matched.placeholder_text(sema, TAB_VAR)?;
        let key = matched.placeholder_text(sema, KEY_VAR)?;
        let default = matched.placeholder_text(sema, DEFAULT_VAR)?;
        let replacement = format!("ets:lookup_element({tab}, {key}, 2, {default})");
        let range = matched.range.range;
        let mut builder = SourceChangeBuilder::new(matched.range.file_id);
        builder.replace(range, replacement);
        let fixes = vec![fix(
            "ets_lookup_to_lookup_element",
            "Rewrite to use ets:lookup_element/4",
            builder.finish(),
            range,
        )];
        Some(fixes)
    }

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }
}

pub(crate) static LINTER: EtsLookupToLookupElementLinter = EtsLookupToLookupElementLinter;

static TAB_VAR: &str = "_@Tab";
static KEY_VAR: &str = "_@Key";
static VAL_VAR: &str = "_@Val";
static DEFAULT_VAR: &str = "_@Default";
static RESULT_KEY_VAR: &str = "_@ResultKey";

fn is_not_a_computation(body: &Body, matched: &PlaceholderMatch) -> bool {
    match matched.code_id {
        SubId::AnyExprId(AnyExprId::Expr(expr_id)) => {
            is_not_a_computation_expr(body, body.exprs[expr_id].clone())
        }
        SubId::AnyExprId(AnyExprId::Pat(_)) => true,
        SubId::Var(_) => true,
        SubId::Atom(_) => true,
        _ => false,
    }
}

fn is_not_a_computation_expr(body: &Body, expr: Expr) -> bool {
    match expr {
        Expr::Literal(_) => true,
        Expr::Var(_) => true,
        Expr::Tuple { exprs: tuple_elems } => tuple_elems
            .iter()
            .all(|elem_id| is_not_a_computation_expr(body, body.exprs[*elem_id].clone())),
        Expr::List {
            exprs: list_elems,
            tail: list_tail,
        } => {
            list_tail
                .is_none_or(|tail_id| is_not_a_computation_expr(body, body.exprs[tail_id].clone()))
                && list_elems
                    .iter()
                    .all(|elem_id| is_not_a_computation_expr(body, body.exprs[*elem_id].clone()))
        }
        _ => false,
    }
}

fn is_wildcard_match(sema: &Semantic, body: &Body, wildcard: &Option<PlaceholderMatch>) -> bool {
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

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::EtsLookupToLookupElement
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    // =================== Detection tests ===================

    #[test]
    fn detects_lookup_with_wildcard_key_and_wildcard_default() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of [{_, V}] -> V; _ -> undefined end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 weak: W0064: Can be rewritten using `ets:lookup_element/4` for improved performance.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn detects_lookup_with_wildcard_key_and_empty_list_default() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of [{_, V}] -> V; [] -> undefined end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 weak: W0064: Can be rewritten using `ets:lookup_element/4` for improved performance.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn detects_lookup_with_empty_list_first() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of [] -> undefined; [{_K, V}] -> V end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 weak: W0064: Can be rewritten using `ets:lookup_element/4` for improved performance.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn detects_lookup_with_matching_key_in_tuple() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of [{Key, V}] -> V; _ -> undefined end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 weak: W0064: Can be rewritten using `ets:lookup_element/4` for improved performance.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn detects_lookup_with_tuple_literal_default() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         cache_lookup(Key) ->
            case ets:lookup(my_cache, Key) of [{_, Value}] -> Value; _ -> {error, not_present} end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 weak: W0064: Can be rewritten using `ets:lookup_element/4` for improved performance.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn detects_lookup_with_variable_default() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key, Default) ->
            case ets:lookup(Tab, Key) of [{_, V}] -> V; _ -> Default end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 weak: W0064: Can be rewritten using `ets:lookup_element/4` for improved performance.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    // =================== Negative tests ===================

    #[test]
    fn ignores_lookup_with_mismatched_key() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key, AnotherKey) ->
            case ets:lookup(Tab, Key) of
                [{AnotherKey, V}] -> V;
                _ -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn ignores_lookup_with_complex_found_body() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of
                [{_, V}] -> {ok, V};
                _ -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn ignores_plain_lookup_without_case() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            ets:lookup(Tab, Key).

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2]).
         lookup(_, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn ignores_lookup_where_comments_might_be_lost() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of
                [{_, V}] -> V; % important comment
                _ -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn ignores_lookup_with_non_literal_default_call() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of
                [{_, V}] -> V;
                _ -> ets:lookup(Tab, Key + 1)
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    // =================== Fix tests ===================

    #[test]
    fn fixes_lookup_with_wildcard_key() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            ca~se ets:lookup(Tab, Key) of
                [{_, V}] -> V;
                _ -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         read(Tab, Key) ->
            ets:lookup_element(Tab, Key, 2, undefined).

         "#]],
        )
    }

    #[test]
    fn fixes_lookup_with_named_wildcard_key() {
        // TODO(T256426988): This should detect the pattern and suggest ets:lookup_element/4,
        // but named wildcards (_IgnoredKey, _Else) are not yet matched by the SSR patterns.
        // Once fixed, convert this back to a check_fix test expecting:
        //   ets:lookup_element(Tab, Key, 2, undefined).
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            case ets:lookup(Tab, Key) of
                [{_IgnoredKey, V}] -> V;
                _Else -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_lookup_with_empty_list_default() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            ca~se ets:lookup(Tab, Key) of
                [{_, V}] -> V;
                [] -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         read(Tab, Key) ->
            ets:lookup_element(Tab, Key, 2, undefined).

         "#]],
        )
    }

    #[test]
    fn fixes_lookup_with_empty_list_first() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            ca~se ets:lookup(Tab, Key) of
                [] -> undefined;
                [{_K, V}] -> V
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         read(Tab, Key) ->
            ets:lookup_element(Tab, Key, 2, undefined).

         "#]],
        )
    }

    #[test]
    fn fixes_lookup_with_matching_key() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, Key) ->
            ca~se ets:lookup(Tab, Key) of
                [{Key, V}] -> V;
                _ -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         read(Tab, Key) ->
            ets:lookup_element(Tab, Key, 2, undefined).

         "#]],
        )
    }

    #[test]
    fn fixes_lookup_with_compound_matching_key() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         read(Tab, K1, K2) ->
            ca~se ets:lookup(Tab, {K1, K2}) of
                [{{K1, K2}, V}] -> V;
                _ -> undefined
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         read(Tab, K1, K2) ->
            ets:lookup_element(Tab, {K1, K2}, 2, undefined).

         "#]],
        )
    }

    #[test]
    fn fixes_lookup_with_macro_table_name() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).
         -define(MY_TABLE, my_table).

         read(Key) ->
            case ets:looku~p(?MY_TABLE, Key) of
                [{_, V}] -> V;
                _ -> none
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).
         -define(MY_TABLE, my_table).

         read(Key) ->
            ets:lookup_element(?MY_TABLE, Key, 2, none).

         "#]],
        )
    }

    #[test]
    fn fixes_lookup_with_tuple_literal_default() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         read(Key) ->
            ca~se ets:lookup(my_tab, Key) of
                [{_, V}] -> V;
                _ -> {error, not_present}
            end.

         //- /src/ets.erl
         -module(ets).
         -export([lookup/2, lookup_element/4]).
         lookup(_, _) -> error(not_impl).
         lookup_element(_, _, _, _) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         read(Key) ->
            ets:lookup_element(my_tab, Key, 2, {error, not_present}).

         "#]],
        )
    }
}
