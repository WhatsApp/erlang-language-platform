/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: unnecessary_map_from_list_around_comprehension
//!
//! warn on code of the form `maps:from_list([{K,V} || {K,V} <- L])` and suggest `#{K => V || {K, V} <- L]}`

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use hir::Semantic;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;
use lazy_static::lazy_static;

use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct UnnecessaryMapFromListAroundComprehensionLinter;

impl Linter for UnnecessaryMapFromListAroundComprehensionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnnecessaryMapFromListAroundComprehension
    }

    fn description(&self) -> &'static str {
        "Unnecessary intermediate list allocated."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GeneratorStrictness {
    Strict,
    NonStrict,
}

impl SsrPatternsLinter for UnnecessaryMapFromListAroundComprehensionLinter {
    type Context = GeneratorStrictness;

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand, // We don't want to rewrite across a macro boundary
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, GeneratorStrictness)> = vec![
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <- {GENERATOR_VAR}])."
                    ),
                    GeneratorStrictness::NonStrict,
                ),
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <- {GENERATOR_VAR}, {COND1_VAR}])."
                    ),
                    GeneratorStrictness::NonStrict,
                ),
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <- {GENERATOR_VAR}, {COND1_VAR}, {COND2_VAR}])."
                    ),
                    GeneratorStrictness::NonStrict,
                ),
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <- {GENERATOR_VAR}, {COND1_VAR}, {COND2_VAR}, {COND3_VAR}])."
                    ),
                    GeneratorStrictness::NonStrict,
                ),
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <:- {GENERATOR_VAR}])."
                    ),
                    GeneratorStrictness::Strict,
                ),
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <:- {GENERATOR_VAR}, {COND1_VAR}])."
                    ),
                    GeneratorStrictness::Strict,
                ),
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <:- {GENERATOR_VAR}, {COND1_VAR}, {COND2_VAR}])."
                    ),
                    GeneratorStrictness::Strict,
                ),
                (
                    format!(
                        "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <:- {GENERATOR_VAR}, {COND1_VAR}, {COND2_VAR}, {COND3_VAR}])."
                    ),
                    GeneratorStrictness::Strict,
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
        if let Some(comments) = matched.comments(sema) {
            // Avoid clobbering comments in the original source code
            if !comments.is_empty() {
                return None;
            }
        }
        Some(true)
    }

    fn fixes(
        &self,
        strictness: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<elp_ide_assists::Assist>> {
        let inefficient_comprehension_range = matched.range.range;
        let binding = matched.placeholder_text(sema, BINDING_VAR)?;
        let key = matched.placeholder_text(sema, KEY_VAR)?;
        let value = matched.placeholder_text(sema, VALUE_VAR)?;
        let generator = matched.placeholder_text(sema, GENERATOR_VAR)?;

        let cond1 = matched.placeholder_text(sema, COND1_VAR);
        let cond2 = matched.placeholder_text(sema, COND2_VAR);
        let cond3 = matched.placeholder_text(sema, COND3_VAR);

        let mut builder = SourceChangeBuilder::new(file_id);
        let conds = vec![cond1, cond2, cond3]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        let conds_str = if !conds.is_empty() {
            format!(", {}", conds.join(", "))
        } else {
            String::new()
        };
        let arrow = match strictness {
            GeneratorStrictness::Strict => "<:-",
            GeneratorStrictness::NonStrict => "<-",
        };
        let direct_comprehension =
            format!("#{{ {key} => {value} || {binding} {arrow} {generator}{conds_str} }}");
        builder.replace(inefficient_comprehension_range, direct_comprehension);
        let fixes = vec![fix(
            "unnecessary_map_from_list_around_comprehension",
            "Rewrite to construct the map directly from the comprehension",
            builder.finish(),
            inefficient_comprehension_range,
        )];
        Some(fixes)
    }
}

pub(crate) static LINTER: UnnecessaryMapFromListAroundComprehensionLinter =
    UnnecessaryMapFromListAroundComprehensionLinter;

static GENERATOR_VAR: &str = "_@Generator";
static BINDING_VAR: &str = "_@Binding";
static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";

static COND1_VAR: &str = "_@Cond1";
static COND2_VAR: &str = "_@Cond2";
static COND3_VAR: &str = "_@Cond3";

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::UnnecessaryMapFromListAroundComprehension
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
    fn ignores_when_element_is_not_syntactically_a_pair() {
        check_diagnostics(
            r#"
         //- /src/unnecessary_maps_from_list_around_comprehension.erl
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_list([Pair || Pair <- List]).
            "#,
        )
    }

    #[test]
    fn detects_unnecessary_maps_from_list_around_comprehension_when_deconstructing_list_elem() {
        check_diagnostics(
            r#"
         //- /src/unnecessary_maps_from_list_around_comprehension.erl
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_list([{K + 1, V + 2} || {K,V} <- List]).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0036: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn detects_unnecessary_maps_from_list_around_comprehension_when_binding_whole_list_elem() {
        check_diagnostics(
            r#"
         //- /src/unnecessary_maps_from_list_around_comprehension.erl
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_list([{element(1, Pair) + 1, element(2, Pair) + 2} || Pair <- List]).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0036: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_unnecessary_maps_from_list_around_comprehension_when_deconstructing_list_elem() {
        check_fix(
            r#"
         //- /src/unnecessary_maps_from_list_around_comprehension.erl
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_l~ist([{K + 1, V + 2} || {K,V} <- List]).
            "#,
            expect![[r#"
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> #{ K + 1 => V + 2 || {K,V} <- List }.
            "#]],
        )
    }

    #[test]
    fn fixes_unnecessary_maps_from_list_around_comprehension_when_binding_whole_list_elem() {
        check_fix(
            r#"
         //- /src/unnecessary_maps_from_list_around_comprehension.erl
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_l~ist([{element(1, Pair) + 1, element(2, Pair) + 2} || Pair <- List]).
            "#,
            expect![[r#"
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> #{ element(1, Pair) + 1 => element(2, Pair) + 2 || Pair <- List }.
            "#]],
        )
    }

    #[test]
    fn fixes_unnecessary_maps_from_list_around_comprehension_with_guard() {
        check_fix(
            r#"
         //- /src/unnecessary_maps_from_list_around_comprehension.erl
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_l~ist([{K + 1, V + 2} || {K,V} <- List, V > 8]).
            "#,
            expect![[r#"
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> #{ K + 1 => V + 2 || {K,V} <- List, V > 8 }.
            "#]],
        )
    }

    #[test]
    fn fixes_unnecessary_maps_from_list_around_comprehension_strict_with_guard() {
        check_fix(
            r#"
         //- /src/unnecessary_maps_from_list_around_comprehension.erl
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> maps:from_l~ist([{K + 1, V + 2} || {K,V} <:- List, V > 8]).
            "#,
            expect![[r#"
         -module(unnecessary_maps_from_list_around_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> #{ K + 1 => V + 2 || {K,V} <:- List, V > 8 }.
            "#]],
        )
    }
}
