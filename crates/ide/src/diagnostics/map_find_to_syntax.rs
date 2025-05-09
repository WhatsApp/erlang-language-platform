/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: map_find_to_syntax
//!
//! offer to rewrite basic maps find calls where the result is immediately
//! inspected to the equivalent syntax, which avoids allocating an
//! intermediate `{ok, V}` tuple.
//!
//! e.g.
//!
//! ```ignore
//! case maps:find(K,M) of
//!     {ok, V} -> A; % Unnecessary allocation of result tuple
//!     error -> B
//! end
//! ```
//!
//! becomes
//!
//! ```ignore
//! case M of
//!     #{K := V} -> A;
//!     #{} -> B
//! end
//! ```

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::PlaceholderMatch;
use elp_ide_ssr::SubId;
use elp_ide_ssr::match_pattern_in_file_functions;
use hir::AnyExprId;
use hir::Body;
use hir::Expr;
use hir::Pat;
use hir::Semantic;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} -> {FOUND_BODY_VAR};
                    error -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    error -> {NOT_FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR}}} -> {FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} when {VALUE_GUARD_VAR} -> {FOUND_BODY_VAR};
                    error -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    error -> {NOT_FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR}}} when {VALUE_GUARD_VAR} -> {FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} -> {FOUND_BODY_VAR};
                    _ -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} -> {FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR2}}} -> {FOUND_BODY_VAR2};
                    error -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} -> {FOUND_BODY_VAR};
                    error -> {NOT_FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR2}}} -> {FOUND_BODY_VAR2}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    error -> {NOT_FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR}}} -> {FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR2}}} -> {FOUND_BODY_VAR2}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} -> {FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR2}}} -> {FOUND_BODY_VAR2};
                    _ -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} when {VALUE_GUARD_VAR} -> {FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR2}}} -> {FOUND_BODY_VAR2};
                    _ -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} when {VALUE_GUARD_VAR} -> {FOUND_BODY_VAR};
                    {{ok, {VALUE_VAR2}}} -> {FOUND_BODY_VAR2};
                    error -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
        from_ssr(
            diags,
            sema,
            file_id,
            format!(
                "ssr: case maps:find({KEY_VAR},{MAP_VAR}) of
                    {{ok, {VALUE_VAR}}} when {VALUE_GUARD_VAR} -> {FOUND_BODY_VAR};
                    _ -> {NOT_FOUND_BODY_VAR}
                  end."
            ),
        );
    },
};

static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";
static VALUE_GUARD_VAR: &str = "_@ValueGuard";
static MAP_VAR: &str = "_@Map";
static FOUND_BODY_VAR: &str = "_@Found";
static NOT_FOUND_BODY_VAR: &str = "_@NotFound";

static VALUE_VAR2: &str = "_@Value2";
static FOUND_BODY_VAR2: &str = "_@Found2";

fn from_ssr(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId, pattern: String) {
    let matches = match_pattern_in_file_functions(
        sema,
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        pattern.as_str(),
    );
    matches.matches.iter().for_each(|m| {
        || -> Option<()> {
            let key = m.get_placeholder_match(sema, KEY_VAR)?;
            let value = m.get_placeholder_match(sema, VALUE_VAR)?;
            let maybe_value2 = m.get_placeholder_match(sema, VALUE_VAR2);
            let body_arc = m.matched_node_body.get_body(sema)?;
            let body = body_arc.as_ref();
            if is_match_valid_pat(body, key) && is_match_valid_pat(body, value) {
                if let Some(value2) = maybe_value2 {
                    if is_match_valid_pat(body, value2) {
                        if let Some(diagnostic) = make_diagnostic(sema, m) {
                            diags.push(diagnostic)
                        }
                    }
                } else if let Some(diagnostic) = make_diagnostic(sema, m) {
                    diags.push(diagnostic)
                }
            }
            Some(())
        }();
    });
}

fn is_match_valid_pat(body: &Body, matched: PlaceholderMatch) -> bool {
    match matched.code_id {
        SubId::AnyExprId(AnyExprId::Expr(expr_id)) => {
            is_expr_valid_pat(body, body.exprs[expr_id].clone())
        }
        SubId::AnyExprId(AnyExprId::Pat(pat_id)) => is_pat_valid(body, body.pats[pat_id].clone()),
        SubId::Var(_) => true,
        SubId::Atom(_) => true,
        _ => false,
    }
}

fn is_expr_valid_pat(body: &Body, expr: Expr) -> bool {
    match expr {
        Expr::Var(_) => true,
        Expr::Literal(_) => true,
        Expr::Tuple { exprs: tuple_elems } => tuple_elems
            .iter()
            .all(|elem_id| is_expr_valid_pat(body, body.exprs[*elem_id].clone())),
        _ => false,
    }
}

fn is_pat_valid(body: &Body, pat: Pat) -> bool {
    match pat {
        Pat::Binary { .. } => true,
        Pat::Literal { .. } => true,
        Pat::Var { .. } => true,
        Pat::Tuple { pats } => pats
            .iter()
            .all(|pat_id| is_pat_valid(body, body.pats[*pat_id].clone())),
        Pat::List { pats, tail } => {
            pats.iter()
                .all(|pat_id| is_pat_valid(body, body.pats[*pat_id].clone()))
                && tail.is_none_or(|tail_pat_id| is_pat_valid(body, body.pats[tail_pat_id].clone()))
        }
        Pat::Map { .. } => false, // Map's expression semantics are different to their pattern semantics (e.g. patterns need match only a subset of the map value, so we explicitly reject them here for simplicity)
        _ => false,
    }
}

fn make_diagnostic(sema: &Semantic, matched: &Match) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let old_query_range = matched.range.range;
    let message = "Unnecessary allocation of result tuple when the key is found.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let map_syntax_replacement = get_map_syntax_replacement(sema, matched)?;
    builder.replace(old_query_range, map_syntax_replacement);
    let fixes = vec![fix(
        "maps_find_rather_than_syntax",
        "Rewrite to use map query syntax",
        builder.finish(),
        old_query_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::MapsFindFunctionRatherThanSyntax,
            message,
            old_query_range,
        )
        .with_severity(Severity::Warning)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes)),
    )
}

fn get_map_syntax_replacement(sema: &Semantic, m: &Match) -> Option<String> {
    let key = m.placeholder_text(sema, KEY_VAR)?;
    let map = m.placeholder_text(sema, MAP_VAR)?;
    let val = m.placeholder_text(sema, VALUE_VAR)?;
    let found = m.placeholder_text(sema, FOUND_BODY_VAR)?;
    let not_found = m.placeholder_text(sema, NOT_FOUND_BODY_VAR)?;

    if let Some(comments) = m.comments(sema) {
        // Avoid clobbering comments in the original source code
        if !comments.is_empty() {
            return None;
        }
    }

    if let Some(val2) = m.placeholder_text(sema, VALUE_VAR2) {
        let found2 = m.placeholder_text(sema, FOUND_BODY_VAR2)?;
        if let Some(val_guard) = m.placeholder_text(sema, VALUE_GUARD_VAR) {
            Some(format!(
                "case {map} of
       #{{{key} := {val}}} when {val_guard} ->
           {found};
       #{{{key} := {val2}}} ->
           {found2};
       #{{}} ->
           {not_found}
   end"
            ))
        } else {
            Some(format!(
                "case {map} of
       #{{{key} := {val}}} ->
           {found};
       #{{{key} := {val2}}} ->
           {found2};
       #{{}} ->
           {not_found}
   end"
            ))
        }
    } else if let Some(val_guard) = m.placeholder_text(sema, VALUE_GUARD_VAR) {
        Some(format!(
            "case {map} of
       #{{{key} := {val}}} when {val_guard} ->
           {found};
       #{{}} ->
           {not_found}
   end"
        ))
    } else {
        Some(format!(
            "case {map} of
       #{{{key} := {val}}} ->
           {found};
       #{{}} ->
           {not_found}
   end"
        ))
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
        d.code == DiagnosticCode::MapsFindFunctionRatherThanSyntax
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
    fn ignores_map_find_where_comments_might_be_lost() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of % don't lose this comment
                {ok, V} ->
                    {found, V};
                error ->
                not_found
            end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, V} -> % don't lose this comment
                    {found, V};
                error ->
                not_found
            end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, V} ->
                    {found, V}; % don't lose this comment
                error ->
                not_found
            end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, V} ->
                    {found, V};
                error -> % don't lose this comment
                    not_found
            end.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, V} ->
                    {found, V};
                error ->
                    % don't lose this comment
                    not_found
            end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_where_the_key_is_not_a_simple_pattern_call() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         get_key() -> [a,b].

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(get_key(), M) of {ok, V} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_where_the_key_is_not_a_simple_pattern_map() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(#{}, M) of {ok, V} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_where_the_key_is_not_a_simple_pattern_tuple() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         get_key() -> [a,b].

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find({tag,get_key()}, M) of {ok, V} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_simple_literal_key() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(my_key,M) of {ok, V} -> Found; error -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_tuple_literal_key() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find({key_a,key_b},M) of {ok, V} -> Found; error -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_immediate_pattern_match_using_error_atom() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(K,M) of {ok, V} -> Found; error -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_immediate_pattern_match_using_underscore() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(K,M) of {ok, V} -> Found; _ -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn rewrites_map_find_with_immediate_pattern_match_using_error_atom_to_syntax() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(K,M) of
                {ok, V} ->
                    Found;
                error ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{K := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_immediate_pattern_match_using_underscore_to_syntax() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(K,M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{K := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_simple_literal_key() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(my_key,M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{my_key := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_literal_tuple_key() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd({key_a,key_b},M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{{key_a,key_b} := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_tuple_pat_key() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd({key_a,KB},M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{{key_a,KB} := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_where_error_is_handled_first() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(my_key,M) of
                error -> NotFound;
                {ok, V} -> Found
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{my_key := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_two_ok_cases_and_underscore() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, a} ->
                    FoundA;
                {ok, b} ->
                    FoundB;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := a} ->
                    FoundA;
                #{key := b} ->
                    FoundB;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_two_ok_cases_and_error() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, a} ->
                    FoundA;
                {ok, b} ->
                    FoundB;
                error ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := a} ->
                    FoundA;
                #{key := b} ->
                    FoundB;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_two_ok_cases_guard_and_underscore() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, A} when is_binary(A) ->
                    FoundA;
                {ok, B} ->
                    FoundB;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := A} when is_binary(A) ->
                    FoundA;
                #{key := B} ->
                    FoundB;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_two_ok_cases_guard_and_error() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, A} when is_binary(A) ->
                    FoundA;
                {ok, B} ->
                    FoundB;
                error ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := A} when is_binary(A) ->
                    FoundA;
                #{key := B} ->
                    FoundB;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_guard() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, V} when is_binary(V) ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := V} when is_binary(V) ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_compound_guard() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, V} when is_binary(V) orelse is_atom(V) ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := V} when is_binary(V) orelse is_atom(V) ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    // Disabled until SSR supports globbing statements
    /*
    #[test]
    fn rewrites_map_find_with_multiline_branches() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, V} when is_integer(V) ->
                    V + 1;
                error ->
                    W = 1,
                    W + W
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := V} when is_integer(V) ->
                    V + 1;
                #{} ->
                    W = 1,
                    W + W
            end.
            "#]],
        )
    }
    */
}
