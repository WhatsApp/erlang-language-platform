/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: unnecessary fold to build map
//!
//! warn on code of the form
//! lists:foldl(fun(K, Acc) -> Acc#{K => []} end, #{}, List)
//! and suggest
//! maps:from_keys(List)
//!
//! warn on code of the form
//! lists:foldl(fun({K,V}, Acc) -> Acc#{K => V} end, #{}, List)
//! and suggest
//! maps:from_list(List)

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::PlaceholderMatch;
use elp_ide_ssr::SubId;
use elp_ide_ssr::is_placeholder_a_var_from_body;
use hir::AnyExprId;
use hir::Body;
use hir::Expr;
use hir::Semantic;

use crate::Assist;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct UnnecessaryFoldToBuildMapLinter;

impl Linter for UnnecessaryFoldToBuildMapLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnnecessaryFoldToBuildMapFromList
    }

    fn description(&self) -> String {
        "Unnecessary explicit fold to construct map.".to_string()
    }

    fn severity(&self) -> Severity {
        Severity::WeakWarning
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PatternKind {
    FromList,
    FromKeys,
}

impl SsrPatternsLinter for UnnecessaryFoldToBuildMapLinter {
    type Context = PatternKind;

    fn patterns(&self) -> Vec<(String, Self::Context)> {
        vec![
            (
                format!(
                    "ssr: lists:foldl(fun({{{KEY_VAR},{VALUE_VAR}}}, {ACC_VAR}) -> {ACC_VAR}#{{{KEY_VAR} => {VALUE_VAR}}} end, #{{}}, {LIST_VAR})."
                ),
                PatternKind::FromList,
            ),
            (
                format!(
                    "ssr: lists:foldl(fun({KEY_VAR}, {ACC_VAR}) -> {ACC_VAR}#{{{KEY_VAR} => {VALUE_VAR}}} end, #{{}}, {LIST_VAR})."
                ),
                PatternKind::FromKeys,
            ),
        ]
    }

    fn pattern_description(&self, context: &Self::Context) -> String {
        let origin = match context {
            PatternKind::FromList => "list".to_string(),
            PatternKind::FromKeys => "keys".to_string(),
        };
        format!("Unnecessary explicit fold to construct map from {origin}.")
    }

    fn is_match_valid(
        &self,
        context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<bool> {
        if matched.range.file_id != file_id {
            // We've somehow ended up with a match in a different file - this means we've
            // accidentally expanded a macro from a different file, or some other complex case that
            // gets hairy, so bail out.
            return None;
        }
        if let Some(comments) = matched.comments(sema) {
            // Avoid clobbering comments in the original source code
            if !comments.is_empty() {
                return None;
            }
        }
        match context {
            PatternKind::FromList => from_list_match_is_valid(sema, matched),
            PatternKind::FromKeys => from_keys_match_is_valid(sema, matched),
        }
    }

    fn fixes(
        &self,
        context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        match context {
            PatternKind::FromList => from_list_fixes(sema, file_id, matched),
            PatternKind::FromKeys => from_keys_fixes(sema, file_id, matched),
        }
    }
}

pub(crate) static LINTER: UnnecessaryFoldToBuildMapLinter = UnnecessaryFoldToBuildMapLinter;

static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";
static ACC_VAR: &str = "_@Acc";
static LIST_VAR: &str = "_@List";

fn from_list_match_is_valid(sema: &Semantic, m: &Match) -> Option<bool> {
    let body_arc = m.matched_node_body.get_body(sema)?;
    let body = body_arc.as_ref();
    let key_matches = m.get_placeholder_matches(sema, KEY_VAR)?;
    let key = key_matches.first()?;
    let value_matches = m.get_placeholder_matches(sema, VALUE_VAR)?;
    let value = value_matches.first()?;
    let acc_matches = m.get_placeholder_matches(sema, ACC_VAR)?;
    let acc = acc_matches.first()?;
    Some(
        is_placeholder_a_var_from_body(body, key)
            && is_placeholder_a_var_from_body(body, value)
            && is_placeholder_a_var_from_body(body, acc),
    )
}

fn from_keys_match_is_valid(sema: &Semantic, m: &Match) -> Option<bool> {
    let body_arc = m.matched_node_body.get_body(sema)?;
    let body = body_arc.as_ref();
    let key_matches = m.get_placeholder_matches(sema, KEY_VAR)?;
    let key = key_matches.first()?;
    let value = &m.get_placeholder_match(sema, VALUE_VAR)?;
    let acc_matches = m.get_placeholder_matches(sema, ACC_VAR)?;
    let acc = acc_matches.first()?;
    Some(
        is_placeholder_a_var_from_body(body, key)
            && is_placeholder_a_var_from_body(body, acc)
            && is_pure(body, value),
    )
}

fn is_pure(body: &Body, matched: &PlaceholderMatch) -> bool {
    match matched.code_id {
        SubId::AnyExprId(AnyExprId::Expr(expr_id)) => {
            is_pure_expr(body, body.exprs[expr_id].clone())
        }
        SubId::AnyExprId(AnyExprId::Pat(_)) => true,
        SubId::Var(_) => true,
        SubId::Atom(_) => true,
        _ => false,
    }
}

fn is_pure_expr(body: &Body, expr: Expr) -> bool {
    match expr {
        Expr::Var(_) => true,
        Expr::Literal(_) => true,
        Expr::Tuple { exprs: tuple_elems } => tuple_elems
            .iter()
            .all(|elem_id| is_pure_expr(body, body.exprs[*elem_id].clone())),
        Expr::List {
            exprs: list_elems,
            tail: list_tail,
        } => {
            list_tail.is_none_or(|tail_id| is_pure_expr(body, body.exprs[tail_id].clone()))
                && list_elems
                    .iter()
                    .all(|elem_id| is_pure_expr(body, body.exprs[*elem_id].clone()))
        }
        _ => false,
    }
}

fn from_list_fixes(sema: &Semantic, file_id: FileId, matched: &Match) -> Option<Vec<Assist>> {
    let unncessary_fold_range = matched.range.range;
    let list_arg = matched.placeholder_text(sema, LIST_VAR)?;
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_last = format!("maps:from_list({list_arg})");
    builder.replace(unncessary_fold_range, efficient_last);
    let fixes = vec![fix(
        "unnecessary_fold_to_build_map_from_list",
        "Rewrite to use maps:from_list/1",
        builder.finish(),
        unncessary_fold_range,
    )];
    Some(fixes)
}

fn from_keys_fixes(sema: &Semantic, file_id: FileId, matched: &Match) -> Option<Vec<Assist>> {
    let unncessary_fold_range = matched.range.range;
    let list_arg = matched.placeholder_text(sema, LIST_VAR)?;
    let value = matched.placeholder_text(sema, VALUE_VAR)?;
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_last = format!("maps:from_keys({list_arg}, {value})");
    builder.replace(unncessary_fold_range, efficient_last);
    let fixes = vec![fix(
        "unnecessary_fold_to_build_map_from_keys",
        "Rewrite to use maps:from_keys/2",
        builder.finish(),
        unncessary_fold_range,
    )];
    Some(fixes)
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::UnnecessaryFoldToBuildMapFromList
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
    fn detects_unnecessary_fold_maps_from_keys() {
        check_diagnostics(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:foldl(fun(K, Acc) -> Acc#{K => []} end, #{}, List).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary explicit fold to construct map from keys.
            "#,
        )
    }

    #[test]
    fn detects_unnecessary_fold_maps_from_list() {
        check_diagnostics(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:foldl(fun({K,V}, Acc) -> Acc#{K => V} end, #{}, List).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary explicit fold to construct map from list.
            "#,
        )
    }

    #[test]
    fn ignores_unnecessary_fold_maps_from_keys_when_filtering() {
        check_diagnostics(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:foldl(fun(key=K, Acc) -> Acc#{K => []} end, #{}, List).
            "#,
        )
    }

    #[test]
    fn ignores_unnecessary_fold_maps_from_list_when_filtering() {
        check_diagnostics(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:foldl(fun({key=K,V}, Acc) -> Acc#{K => V} end, #{}, List).
            "#,
        )
    }

    #[test]
    fn ignores_unnecessary_fold_maps_from_keys_when_mapping() {
        check_diagnostics(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:foldl(fun(K, Acc) -> Acc#{K + 1 => []} end, #{}, List).
            "#,
        )
    }

    #[test]
    fn ignores_unnecessary_fold_maps_from_keys_when_value_is_not_constant() {
        check_diagnostics(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:foldl(fun(K, Acc) -> Acc#{K => erlang:localtime()} end, #{}, List).
            "#,
        )
    }

    #[test]
    fn ignores_unnecessary_fold_maps_from_list_when_mapping() {
        check_diagnostics(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:foldl(fun({K,V}, Acc) -> Acc#{K => V + 1} end, #{}, List).
            "#,
        )
    }

    #[test]
    fn fixes_unnecessary_fold_maps_from_keys_literal() {
        check_fix(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:fo~ldl(fun(K, Acc) -> Acc#{K => []} end, #{}, List).
            "#,
            expect![[r#"
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_keys(List, []).
            "#]],
        )
    }

    #[test]
    fn fixes_unnecessary_fold_maps_from_keys_var() {
        check_fix(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List, Value) -> lists:fo~ldl(fun(K, Acc) -> Acc#{K => Value} end, #{}, List).
            "#,
            expect![[r#"
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List, Value) -> maps:from_keys(List, Value).
            "#]],
        )
    }

    #[test]
    fn fixes_unnecessary_fold_maps_from_keys_composite() {
        check_fix(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List, Value) -> lists:fo~ldl(fun(K, Acc) -> Acc#{K => {value, Value}} end, #{}, List).
            "#,
            expect![[r#"
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List, Value) -> maps:from_keys(List, {value, Value}).
            "#]],
        )
    }

    #[test]
    fn fixes_unnecessary_fold_maps_from_list() {
        check_fix(
            r#"
         //- /src/unneccessary_fold_to_build_map.erl
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:fo~ldl(fun({K,V}, Acc) -> Acc#{K => V} end, #{}, List).
            "#,
            expect![[r#"
         -module(unneccessary_fold_to_build_map).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_list(List).
            "#]],
        )
    }
}
