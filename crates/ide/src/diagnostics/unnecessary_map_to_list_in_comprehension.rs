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
use hir::Semantic;

use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct UnnecessaryMapToListInComprehensionLinter;

impl Linter for UnnecessaryMapToListInComprehensionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnnecessaryMapToListInComprehension
    }

    fn description(&self) -> String {
        "Unnecessary intermediate list allocated.".to_string()
    }

    fn severity(&self) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for UnnecessaryMapToListInComprehensionLinter {
    type Context = ();

    fn patterns(&self) -> Vec<(String, Self::Context)> {
        vec![(
            format!("ssr: [{BODY_VAR} || {{{KEY_VAR}, {VALUE_VAR}}} <- maps:to_list({MAP_VAR})]."),
            (),
        )]
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<bool> {
        if let Some(comments) = matched.comments(sema) {
            // Avoid clobbering comments in the original source code
            if !comments.is_empty() {
                return None;
            }
        }
        if matched.range.file_id != file_id {
            // We've somehow ended up with a match in a different file - this means we've
            // accidentally expanded a macro from a different file, or some other complex case that
            // gets hairy, so bail out.
            return None;
        }
        Some(true)
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<elp_ide_assists::Assist>> {
        let inefficient_comprehension_range = matched.range.range;
        let body = matched.placeholder_text(sema, BODY_VAR)?;
        let key = matched.placeholder_text(sema, KEY_VAR)?;
        let value = matched.placeholder_text(sema, VALUE_VAR)?;
        let map = matched.placeholder_text(sema, MAP_VAR)?;

        let mut builder = SourceChangeBuilder::new(file_id);
        let direct_comprehension = format!("[{body} || {key} := {value} <- {map}]");
        builder.replace(inefficient_comprehension_range, direct_comprehension);
        let fixes = vec![fix(
            "unnecessary_map_to_list_in_comprehension",
            "Rewrite to iterate over the map directly",
            builder.finish(),
            inefficient_comprehension_range,
        )];
        Some(fixes)
    }
}

pub(crate) static LINTER: UnnecessaryMapToListInComprehensionLinter =
    UnnecessaryMapToListInComprehensionLinter;

static MAP_VAR: &str = "_@Map";
static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";
static BODY_VAR: &str = "_@Body";

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
