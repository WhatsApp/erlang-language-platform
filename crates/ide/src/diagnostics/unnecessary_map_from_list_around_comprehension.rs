/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: unnecessary_map_from_list_around_comprehension
//!
//! warn on code of the form `maps:from_list([{K,V} || {K,V} <- L])` and suggest `#{K => V || {K, V} <- L]}`

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::match_pattern_in_file_functions;
use hir::Semantic;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;

use crate::diagnostics::Category;
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
    checker: &|acc, sema, file_id, _ext| {
        unnecessary_maps_from_list_around_comprehension_ssr(acc, sema, file_id);
    },
};

static GENERATOR_VAR: &str = "_@Generator";
static BINDING_VAR: &str = "_@Binding";
static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";

fn unnecessary_maps_from_list_around_comprehension_ssr(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    let matches = match_pattern_in_file_functions(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!(
            "ssr: maps:from_list([{{{KEY_VAR},{VALUE_VAR}}} || {BINDING_VAR} <- {GENERATOR_VAR}])."
        )
        .as_str(),
    );
    matches.matches.iter().for_each(|m| {
        if let Some(diagnostic) = make_diagnostic(sema, m) {
            diags.push(diagnostic);
        }
    });
}

fn make_diagnostic(sema: &Semantic, matched: &Match) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let inefficient_comprehension_range = matched.range.range;
    let generator = matched.placeholder_text(sema, GENERATOR_VAR)?;
    let key = matched.placeholder_text(sema, KEY_VAR)?;
    let value = matched.placeholder_text(sema, VALUE_VAR)?;
    let binding = matched.placeholder_text(sema, BINDING_VAR)?;
    let message = "Unnecessary intermediate list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let direct_comprehension = format!("#{{ {key} => {value} || {binding} <- {generator} }}");
    builder.replace(inefficient_comprehension_range, direct_comprehension);
    let fixes = vec![fix(
        "unnecessary_map_from_list_around_comprehension",
        "Rewrite to construct the map directly from the comprehension",
        builder.finish(),
        inefficient_comprehension_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::UnnecessaryMapFromListAroundComprehension,
            message,
            inefficient_comprehension_range,
        )
        .with_severity(Severity::WeakWarning)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes)),
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
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary intermediate list allocated.
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
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary intermediate list allocated.
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
}
