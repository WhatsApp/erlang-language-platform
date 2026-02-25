/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: map_function_to_syntax
//!
//! offer to rewrite basic maps function calls to the equivalent syntax
//! e.g. maps:put(K, V, M) becomes M#{K => V}
//!      maps:update(K, V, M) becomes M#{K := V}

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::is_placeholder_a_var_from_sema_and_match;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::Category;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";
static MAP_VAR: &str = "_@Map";

// ---------- maps:put ----------

pub(crate) struct MapPutToSyntaxLinter;

impl Linter for MapPutToSyntaxLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MapsPutFunctionRatherThanSyntax
    }

    fn description(&self) -> &'static str {
        "Consider using map syntax rather than a function call."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for MapPutToSyntaxLinter {
    type Context = ();

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> = vec![(
                format!("ssr: maps:put({KEY_VAR},{VALUE_VAR},{MAP_VAR})."),
                (),
            )];
        }
        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<bool> {
        if matched.range.file_id != file_id {
            return None;
        }
        let map_match = matched.get_placeholder_match(sema, MAP_VAR)?;
        Some(is_placeholder_a_var_from_sema_and_match(
            sema, matched, &map_match,
        ))
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let range = matched.range.range;
        let map = matched.placeholder_text(sema, MAP_VAR)?;
        let key = matched.placeholder_text(sema, KEY_VAR)?;
        let value = matched.placeholder_text(sema, VALUE_VAR)?;
        let mut builder = SourceChangeBuilder::new(file_id);
        builder.replace(range, format!("{map}#{{{key} => {value}}}"));
        Some(vec![fix(
            "maps_put_function_rather_than_syntax",
            "Rewrite to use map put syntax",
            builder.finish(),
            range,
        )])
    }

    fn add_categories(&self, _context: &Self::Context) -> Vec<Category> {
        vec![Category::SimplificationRule]
    }
}

pub(crate) static MAP_PUT_LINTER: MapPutToSyntaxLinter = MapPutToSyntaxLinter;

// ---------- maps:update ----------

pub(crate) struct MapUpdateToSyntaxLinter;

impl Linter for MapUpdateToSyntaxLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MapsUpdateFunctionRatherThanSyntax
    }

    fn description(&self) -> &'static str {
        "Consider using map syntax rather than a function call."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for MapUpdateToSyntaxLinter {
    type Context = ();

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> = vec![(
                format!("ssr: maps:update({KEY_VAR},{VALUE_VAR},{MAP_VAR})."),
                (),
            )];
        }
        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<bool> {
        if matched.range.file_id != file_id {
            return None;
        }
        let map_match = matched.get_placeholder_match(sema, MAP_VAR)?;
        Some(is_placeholder_a_var_from_sema_and_match(
            sema, matched, &map_match,
        ))
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let range = matched.range.range;
        let map = matched.placeholder_text(sema, MAP_VAR)?;
        let key = matched.placeholder_text(sema, KEY_VAR)?;
        let value = matched.placeholder_text(sema, VALUE_VAR)?;
        let mut builder = SourceChangeBuilder::new(file_id);
        builder.replace(range, format!("{map}#{{{key} := {value}}}"));
        Some(vec![fix(
            "maps_update_function_rather_than_syntax",
            "Rewrite to use map update syntax",
            builder.finish(),
            range,
        )])
    }

    fn add_categories(&self, _context: &Self::Context) -> Vec<Category> {
        vec![Category::SimplificationRule]
    }
}

pub(crate) static MAP_UPDATE_LINTER: MapUpdateToSyntaxLinter = MapUpdateToSyntaxLinter;

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::MapsPutFunctionRatherThanSyntax
            || d.code == DiagnosticCode::MapsUpdateFunctionRatherThanSyntax
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

    #[test]
    fn ignores_map_put_function_when_map_is_not_a_var() {
        check_diagnostics(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V) -> maps:put(K, V, #{}).
            "#,
        )
    }

    #[test]
    fn ignores_map_update_function_when_map_is_not_a_var() {
        check_diagnostics(
            r#"
         //- /src/map_update_function.erl
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V) -> maps:update(K, V, #{a => b}).
            "#,
        )
    }

    #[test]
    fn ignores_map_update_function_in_macro() {
        check_diagnostics(
            r#"
         //- /src/map_update_function.erl
         -module(map_update_function).

         -define(MAPS_UPDATE(K,V,M), maps:update(K, V, M)).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, M) -> ?MAPS_UPDATE(K, V, M).
            "#,
        )
    }

    #[test]
    fn detects_map_put_function() {
        check_diagnostics(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:put(K, V, Map).
         %%               ^^^^^^^^^^^^^^^^^^^💡 weak: W0030: Consider using map syntax rather than a function call.
            "#,
        )
    }

    #[test]
    fn detects_map_update_function() {
        check_diagnostics(
            r#"
         //- /src/map_update_function.erl
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:update(K, V, Map).
         %%               ^^^^^^^^^^^^^^^^^^^^^^💡 weak: W0031: Consider using map syntax rather than a function call.
            "#,
        )
    }

    #[test]
    fn rewrites_map_put_function_to_use_syntax() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:p~ut(K, V, Map).
            "#,
            expect![[r#"
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> Map#{K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_update_function_to_use_syntax() {
        check_fix(
            r#"
         //- /src/map_update_function.erl
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:up~date(K, V, Map).
            "#,
            expect![[r#"
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> Map#{K := V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_when_map_is_a_local_binding() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         fn(K, V) ->
            Map = #{a => b},
            % elp:ignore W0017 (undefined_function)
            maps:p~ut(K, V, Map).
            "#,
            expect![[r#"
         -module(map_put_function).

         fn(K, V) ->
            Map = #{a => b},
            % elp:ignore W0017 (undefined_function)
            Map#{K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_when_map_key_and_value_are_local_bindings() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         fn() ->
            Map = #{a => b},
            K = key,
            V = value,
            % elp:ignore W0017 (undefined_function)
            maps:p~ut(K, V, Map).
            "#,
            expect![[r#"
         -module(map_put_function).

         fn() ->
            Map = #{a => b},
            K = key,
            V = value,
            % elp:ignore W0017 (undefined_function)
            Map#{K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_when_map_is_a_local_binding_and_key_and_value_are_literals() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         fn() ->
            Map = #{a => b},
            % elp:ignore W0017 (undefined_function)
            maps:p~ut(key, value, Map).
            "#,
            expect![[r#"
         -module(map_put_function).

         fn() ->
            Map = #{a => b},
            % elp:ignore W0017 (undefined_function)
            Map#{key => value}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_when_there_is_some_preable_before_the_map() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         -define(FOO, foo).

         fn(K, V) ->
            Map = #{a => ?FOO},
            3 = length([a,b,c]),
            1 = map_size(#{x => y}),
            % elp:ignore W0017 (undefined_function)
            maps:p~ut(K, V, Map).
            "#,
            expect![[r#"
         -module(map_put_function).

         -define(FOO, foo).

         fn(K, V) ->
            Map = #{a => ?FOO},
            3 = length([a,b,c]),
            1 = map_size(#{x => y}),
            % elp:ignore W0017 (undefined_function)
            Map#{K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_when_there_are_statements_before_and_after_the_map() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         -define(FOO, foo).

         -spec fn(term(), term()) -> {ok, term()}.
         fn(K, V) ->
            MyParamsMap = #{a => ?FOO},
            3 = length([a,b,c]),
            1 = map_size(#{x => y}),
            % elp:ignore W0017 (undefined_function)
            MyParamsMap2 = maps:p~ut(K, V, MyParamsMap),
            {ok, MyParamsMap2}.
            "#,
            expect![[r#"
         -module(map_put_function).

         -define(FOO, foo).

         -spec fn(term(), term()) -> {ok, term()}.
         fn(K, V) ->
            MyParamsMap = #{a => ?FOO},
            3 = length([a,b,c]),
            1 = map_size(#{x => y}),
            % elp:ignore W0017 (undefined_function)
            MyParamsMap2 = MyParamsMap#{K => V},
            {ok, MyParamsMap2}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_as_sub_expression() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         fn(K, V, Map) ->
            % elp:ignore W0017 (undefined_function)
            tuple_to_list( {ok, maps:pu~t(K, V, Map), 1 + 2}).
            "#,
            expect![[r#"
         -module(map_put_function).

         fn(K, V, Map) ->
            % elp:ignore W0017 (undefined_function)
            tuple_to_list( {ok, Map#{K => V}, 1 + 2}).
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_as_sub_expression_of_multiline_expression() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         fn(K, V, Map) ->
            % elp:ignore W0017 (undefined_function)
            tuple_to_list(
                % elp:ignore W0017 (undefined_function)
                {ok, maps:pu~t(K, V, Map), 1 + 2}
            ).
            "#,
            expect![[r#"
         -module(map_put_function).

         fn(K, V, Map) ->
            % elp:ignore W0017 (undefined_function)
            tuple_to_list(
                % elp:ignore W0017 (undefined_function)
                {ok, Map#{K => V}, 1 + 2}
            ).
            "#]],
        )
    }

    #[test]
    fn rewrites_map_put_function_in_lambda() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         fn(Map) ->
            % elp:ignore W0017 (undefined_function)
            maps:fold(fun(K, V, MapAcc) -> maps:pu~t(K, V, MapAcc) end, #{}, Map).
            "#,
            expect![[r#"
         -module(map_put_function).

         fn(Map) ->
            % elp:ignore W0017 (undefined_function)
            maps:fold(fun(K, V, MapAcc) -> MapAcc#{K => V} end, #{}, Map).
            "#]],
        )
    }
}
