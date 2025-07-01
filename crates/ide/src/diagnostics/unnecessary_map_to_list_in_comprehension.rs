/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: unnecessary_map_to_list_in_comprehension
//!
//! warn on code of the form `[A || {K,V} <- maps:to_list(M)]` and suggest `[A || K := V <- M]`

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::match_pattern_in_file_functions;
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
    let matches = match_pattern_in_file_functions(
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
        if let Some(diagnostic) = make_diagnostic(sema, file_id, m) {
            diags.push(diagnostic);
        }
    });
}

fn make_diagnostic(
    sema: &Semantic,
    original_file_id: FileId,
    matched: &Match,
) -> Option<Diagnostic> {
    if let Some(comments) = matched.comments(sema) {
        // Avoid clobbering comments in the original source code
        if !comments.is_empty() {
            return None;
        }
    }
    let file_id = matched.range.file_id;
    if matched.range.file_id != original_file_id {
        // We've somehow ended up with a match in a different file - this means we've
        // accidentally expanded a macro from a different file, or some other complex case that
        // gets hairy, so bail out.
        return None;
    }
    let inefficient_comprehension_range = matched.range.range;
    let body = matched.placeholder_text(sema, BODY_VAR)?;
    let key = matched.placeholder_text(sema, KEY_VAR)?;
    let value = matched.placeholder_text(sema, VALUE_VAR)?;
    let map = matched.placeholder_text(sema, MAP_VAR)?;
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
    Some(
        Diagnostic::new(
            DiagnosticCode::UnnecessaryMapToListInComprehension,
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
        d.code == DiagnosticCode::UnnecessaryMapToListInComprehension
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
         %%         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary intermediate list allocated.
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
