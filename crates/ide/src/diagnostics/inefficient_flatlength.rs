/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: length_lists_flatten_to_lists_flatlength
//!
//! warn on code of the form `length(lists:flatten(L))` and suggest `lists:flatlength(L)`

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct InefficientFlatlengthLinter;

impl Linter for InefficientFlatlengthLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnnecessaryFlatteningToFindFlatLength
    }

    fn description(&self) -> &'static str {
        "Unnecessary intermediate flat-list allocated."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for InefficientFlatlengthLinter {
    type Context = ();

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> =
                vec![(format!("ssr: length(lists:flatten({LIST_ARG_VAR}))."), ()),];
        }
        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<bool> {
        if let Some(comments) = matched.comments(sema)
            && !comments.is_empty()
        {
            return None;
        }
        Some(true)
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let inefficient_call_range = matched.range.range;
        let nested_list_arg_match_src = matched.placeholder_text(sema, LIST_ARG_VAR)?;
        let mut builder = SourceChangeBuilder::new(file_id);
        let efficient_flatlength = format!("lists:flatlength({nested_list_arg_match_src})");
        builder.replace(inefficient_call_range, efficient_flatlength);
        Some(vec![fix(
            "unnecessary_flattening_to_find_flat_length",
            "Rewrite to use lists:flatlength/1",
            builder.finish(),
            inefficient_call_range,
        )])
    }
}

pub(crate) static LINTER: InefficientFlatlengthLinter = InefficientFlatlengthLinter;

static LIST_ARG_VAR: &str = "_@List";

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::UnnecessaryFlatteningToFindFlatLength
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
    fn detects_inefficient_flatlength() {
        check_diagnostics(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         fn(NestedList) -> length(lists:flatten(NestedList)).
         %%                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0028: Unnecessary intermediate flat-list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_flatlength_over_length_call() {
        check_fix(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> len~gth(lists:flatten(NestedList)).
            "#,
            expect![[r#"
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> lists:flatlength(NestedList).
            "#]],
        )
    }

    #[test]
    fn fixes_inefficient_flatlength_over_flatten_call() {
        check_fix(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> length(lists:fl~atten(NestedList)).
            "#,
            expect![[r#"
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> lists:flatlength(NestedList).
            "#]],
        )
    }
}
