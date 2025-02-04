/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: unnecessary_map_to_list_in_comprehension
//!
//! warn on code of the form `[A || {K,V} <- maps:to_list(M)]` and suggest `[A || K := V <- M]`

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::DiagnosticCode;
use elp_ide_ssr::match_pattern_in_file;
use elp_ide_ssr::Match;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;
use hir::Semantic;

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
        unnecessary_map_to_list_in_comprehension_ssr(acc, sema, file_id);
    },
};

static MAP_VAR: &str = "_@Map";
static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";
static BODY_VAR: &str = "_@Body";

fn unnecessary_map_to_list_in_comprehension_ssr(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    let matches = match_pattern_in_file(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!("ssr: [{BODY_VAR} || {{{KEY_VAR}, {VALUE_VAR}}} <- maps:to_list({MAP_VAR})].")
            .as_str(),
    );
    matches.matches.iter().for_each(|m| {
        let diagnostic = make_diagnostic(sema, m);
        diags.push(diagnostic);
    });
}

fn make_diagnostic(sema: &Semantic, matched: &Match) -> Diagnostic {
    let file_id = matched.range.file_id;
    let inefficient_comprehension_range = matched.range.range;
    let body = matched.placeholder_text(sema, BODY_VAR).unwrap();
    let key = matched.placeholder_text(sema, KEY_VAR).unwrap();
    let value = matched.placeholder_text(sema, VALUE_VAR).unwrap();
    let map = matched.placeholder_text(sema, MAP_VAR).unwrap();
    let message = "Unnecessary intermediate list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let direct_comprehension = format!("[{body} || {key} := {value} <- {map}]");
    builder.replace(inefficient_comprehension_range, direct_comprehension);
    let fixes = vec![fix(
        "unnecessary_map_to_list_in_comprehension",
        "Rewrite to iterate over the map directly",
        builder.finish(),
        inefficient_comprehension_range,
    )];
    Diagnostic::new(
        DiagnosticCode::ExpressionCanBeOptimised,
        message,
        inefficient_comprehension_range,
    )
    .with_severity(Severity::Warning)
    .with_ignore_fix(sema, file_id)
    .with_fixes(Some(fixes))
    .add_categories([Category::SimplificationRule])
}

#[cfg(test)]
mod tests {

    use expect_test::expect;
    use expect_test::Expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::ExpressionCanBeOptimised
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
    fn detects_unnecessary_maps_to_list_in_comprehension() {
        check_diagnostics(
            r#"
         //- /src/unnecessary_maps_to_list_in_comprehension.erl
         -module(unnecessary_maps_to_list_in_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> [K + V + 1 || {K,V} <- maps:to_list(Map)].
         %%         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_unnecessary_maps_to_list_in_comprehension() {
        check_fix(
            r#"
         //- /src/unnecessary_maps_to_list_in_comprehension.erl
         -module(unnecessary_maps_to_list_in_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> [K + V + 1 || {K,V} <- maps:to_~list(Map)].
            "#,
            expect![[r#"
         -module(unnecessary_maps_to_list_in_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(Map) -> [K + V + 1 || K := V <- Map].
            "#]],
        )
    }
}
