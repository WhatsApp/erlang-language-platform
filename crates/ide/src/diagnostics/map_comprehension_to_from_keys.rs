/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: map_comprehension_to_from_keys
//!
//! warn on code of the form
//! #{K => Value || K <- List}
//! and suggest
//! maps:from_keys(List, Value)
//!
//! `maps:from_keys/2` evaluates `Value` once, even for an empty list, so
//! `Value` must be built only from literals and variables other than `K`.

use std::sync::LazyLock;

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::PlaceholderMatch;
use elp_ide_ssr::SubId;
use elp_syntax::ast::UnaryOp;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::BinarySeg;
use hir::Body;
use hir::Expr;
use hir::ExprId;
use hir::Literal;
use hir::Semantic;
use hir::Var;
use hir::fold::FoldCtx;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;

use crate::Assist;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct MapComprehensionToFromKeysLinter;

impl Linter for MapComprehensionToFromKeysLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MapComprehensionToFromKeys
    }

    fn description(&self) -> &'static str {
        "Map comprehension can be replaced with a call to maps:from_keys/2."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for MapComprehensionToFromKeysLinter {
    type Context = ();

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand, // We don't want to rewrite across a macro boundary
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        static PATTERNS: LazyLock<Vec<(String, ())>> = LazyLock::new(|| {
            ["<-", "<:-"]
                .into_iter()
                .map(|arrow| {
                    (
                        format!(
                            "ssr: #{{{KEY_VAR} => {VALUE_VAR} || {KEY_VAR} {arrow} {LIST_VAR}}}."
                        ),
                        (),
                    )
                })
                .collect()
        });

        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<bool> {
        if let Some(comments) = matched.comments_outside_placeholders(ctx.sema)
            && !comments.is_empty()
        {
            return None;
        }
        let body_arc = matched.matched_node_body.get_body(ctx.sema)?;
        let body = body_arc.as_ref();
        // A generator pattern other than a plain variable skips the elements
        // it does not match, so the list is not the set of keys.
        let key_vars = matched
            .get_placeholder_matches(KEY_VAR)?
            .iter()
            .map(|key| key.is_var(body))
            .collect::<Option<Vec<_>>>()?;
        let key_var = *key_vars.first()?;
        let value = matched.get_placeholder_match(VALUE_VAR)?;
        let list = matched.get_placeholder_match(LIST_VAR)?;
        let Some(SubId::AnyExprId(AnyExprId::Expr(list_id))) = list.code_id() else {
            return Some(false);
        };
        Some(is_constant_value(body, &value, key_var) && !binds_variables(body, *list_id))
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let comprehension_range = matched.range.range;
        let list = matched.placeholder_text(ctx.sema, LIST_VAR)?;
        let value = matched.placeholder_text(ctx.sema, VALUE_VAR)?;
        let mut builder = SourceChangeBuilder::new(ctx.file_id);
        builder.replace(
            comprehension_range,
            format!("maps:from_keys({list}, {value})"),
        );
        Some(vec![fix(
            "map_comprehension_to_from_keys",
            "Rewrite to use maps:from_keys/2",
            builder.finish(),
            comprehension_range,
        )])
    }
}

pub(crate) static LINTER: MapComprehensionToFromKeysLinter = MapComprehensionToFromKeysLinter;

static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";
static LIST_VAR: &str = "_@List";

fn is_constant_value(body: &Body, matched: &PlaceholderMatch, key_var: Var) -> bool {
    match matched.code_id() {
        Some(SubId::AnyExprId(AnyExprId::Expr(expr_id))) => is_constant(body, *expr_id, key_var),
        Some(SubId::Var(var)) => *var != key_var,
        Some(SubId::Atom(_)) => true,
        _ => false,
    }
}

// Unlike the value check of `unnecessary_fold_to_build_map`, this also accepts
// signed numbers, binaries, maps, macro calls and parentheses, and rejects the
// generator variable.
fn is_constant(body: &Body, expr_id: ExprId, key_var: Var) -> bool {
    let constant = |elem_id: &ExprId| is_constant(body, *elem_id, key_var);
    match &body.exprs[expr_id] {
        Expr::Var(var) => *var != key_var,
        Expr::Literal(_) => true,
        Expr::UnaryOp {
            expr,
            op: UnaryOp::Minus | UnaryOp::Plus,
        } => matches!(
            body.exprs[*expr],
            Expr::Literal(Literal::Integer(_) | Literal::Float(_))
        ),
        Expr::Binary { segs } => segs.iter().all(|seg| is_literal_segment(body, seg)),
        Expr::Tuple { exprs } => exprs.iter().all(constant),
        Expr::List { exprs, tail } => exprs.iter().chain(tail.iter()).all(constant),
        Expr::Map { fields } => fields
            .iter()
            .all(|(key, value)| constant(key) && constant(value)),
        Expr::MacroCall { expansion, .. } => constant(expansion),
        Expr::Paren { expr } => constant(expr),
        _ => false,
    }
}

// Variables that the generator expression binds are local to the comprehension,
// but stay bound for the rest of the clause once the fix moves the expression
// into a call argument. Conservatively, this also rejects bindings inside a
// nested fun or comprehension, which would stay local.
fn binds_variables(body: &Body, expr_id: ExprId) -> bool {
    let strategy = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::InvisibleParens,
    };
    FoldCtx::fold_expr(strategy, body, expr_id, false, &mut |acc, ctx| {
        acc || matches!(
            ctx.item,
            AnyExpr::Expr(
                Expr::Match { .. }
                    | Expr::Case { .. }
                    | Expr::Receive { .. }
                    | Expr::Try { .. }
                    | Expr::Maybe { .. }
            )
        )
    })
}

// Without a size, unit or type, a segment holds the bytes of a string or an
// 8-bit integer, which cannot fail. With a type, even a literal can fail:
// `<<16#110000/utf8>>` raises `badarg`.
fn is_literal_segment(body: &Body, seg: &BinarySeg<ExprId>) -> bool {
    seg.size.is_none()
        && seg.unit.is_none()
        && seg.tys.is_empty()
        && matches!(
            body.exprs[seg.elem],
            Expr::Literal(Literal::String(_) | Literal::Integer(_) | Literal::Char(_))
        )
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::MapComprehensionToFromKeys
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
    fn detects_literal_value() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List) -> #{K => true || K <- List}.
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                  | 💡 Rewrite to use maps:from_keys/2
         %%                                  | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn detects_composite_value_with_strict_generator() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List, V) -> #{K => {value, [V]} || K <:- List}.
         %%             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                              | 💡 Rewrite to use maps:from_keys/2
         %%                                              | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn detects_map_and_macro_values() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         -define(NO_CHANGES, #{}).

         fn(List, V) -> #{K => #{set => V} || K <- List}.
         %%             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                            | 💡 Rewrite to use maps:from_keys/2
         %%                                            | 💡 <suppression>

         gn(List) -> #{K => ?NO_CHANGES || K <- List}.
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                         | 💡 Rewrite to use maps:from_keys/2
         %%                                         | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn detects_signed_number_and_binary_values() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List) -> #{K => {-1, +2.5} || K <- List}.
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                        | 💡 Rewrite to use maps:from_keys/2
         %%                                        | 💡 <suppression>

         gn(List) -> #{K => <<"abc", 1, $d>> || K <- List}.
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                              | 💡 Rewrite to use maps:from_keys/2
         %%                                              | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn detects_parenthesised_sub_terms() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         -define(TRUE, (true)).

         fn(List, V) -> #{K => {(V), [(1)], #{set => (V)}} || K <- List}.
         %%             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                                            | 💡 Rewrite to use maps:from_keys/2
         %%                                                            | 💡 <suppression>

         gn(List) -> #{K => ?TRUE || K <- List}.
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                   | 💡 Rewrite to use maps:from_keys/2
         %%                                   | 💡 <suppression>

         hn(List) -> #{K => {(K)} || K <- List}.
            "#,
        )
    }

    #[test]
    fn detects_generator_expression_with_inner_comprehension() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(L) -> #{K => true || K <- [M || {ok, M} <- L]}.
         %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ weak: W0096: Map comprehension can be replaced with a call to maps:from_keys/2.
         %%                                              | 💡 Rewrite to use maps:from_keys/2
         %%                                              | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn ignores_generator_expression_that_binds_variables() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(L) -> M = #{K => true || K <- (X = L)}, X = [b], M.

         gn(L) -> #{K => true || K <- case L of [] -> Y = []; _ -> Y = L end}.

         hn(L) -> #{K => true || K <- begin Z = L, Z end}.
            "#,
        )
    }

    #[test]
    fn ignores_signed_and_binary_values_that_can_fail() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List, V) -> {#{K => -a || K <- List}, #{K => -V || K <- List}, #{K => -K || K <- List}}.

         gn(List, V) -> {#{K => <<V/binary>> || K <- List}, #{K => <<16#110000/utf8>> || K <- List}}.

         hn(List) -> {#{K => <<1.5>> || K <- List}, #{K => <<1:16>> || K <- List}}.
            "#,
        )
    }

    #[test]
    fn ignores_value_that_mentions_key() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         -define(WRAP(X), {wrap, X}).

         fn(List) -> {#{K => K || K <- List}, #{K => {value, [K]} || K <- List}}.

         gn(List) -> {#{K => #{key => K} || K <- List}, #{K => ?WRAP(K) || K <- List}}.
            "#,
        )
    }

    #[test]
    fn ignores_impure_value() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> {#{K => erlang:localtime() || K <- List}, #{K => #{at => erlang:localtime()} || K <- List}}.
            "#,
        )
    }

    #[test]
    fn ignores_generator_pattern() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List) -> {#{K => true || {K, _} <- List}, #{{K} => true || {K} <- List}}.
            "#,
        )
    }

    #[test]
    fn ignores_key_expression() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List) -> #{{key, K} => true || K <- List}.
            "#,
        )
    }

    #[test]
    fn ignores_filter() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List) -> #{K => true || K <- List, is_atom(K)}.
            "#,
        )
    }

    #[test]
    fn ignores_map_generator() {
        check_diagnostics(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(Map) -> #{K => true || K := _ <- Map}.
            "#,
        )
    }

    #[test]
    fn fixes_literal_value() {
        check_fix(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         fn(List) -> #{K~ => [] || K <- List}.
         "#,
            expect![[r#"
                -module(map_comprehension_to_from_keys).

                fn(List) -> maps:from_keys(List, []).
   "#]],
        )
    }

    #[test]
    fn fixes_composite_value_with_strict_generator() {
        check_fix(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         % elp:ignore W0017 (undefined_function)
         fn(N, V) -> #{K~ => {value, V} || K <:- lists:seq(1, N)}.
         "#,
            expect![[r#"
                -module(map_comprehension_to_from_keys).

                % elp:ignore W0017 (undefined_function)
                fn(N, V) -> maps:from_keys(lists:seq(1, N), {value, V}).
   "#]],
        )
    }

    #[test]
    fn fixes_macro_value() {
        check_fix(
            r#"
         //- /src/map_comprehension_to_from_keys.erl
         -module(map_comprehension_to_from_keys).

         -define(NO_CHANGES, #{}).

         fn(List) -> #{K~ => ?NO_CHANGES || K <- List}.
         "#,
            expect![[r#"
                -module(map_comprehension_to_from_keys).

                -define(NO_CHANGES, #{}).

                fn(List) -> maps:from_keys(List, ?NO_CHANGES).
            "#]],
        )
    }
}
