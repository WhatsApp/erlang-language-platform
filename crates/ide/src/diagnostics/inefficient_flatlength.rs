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

use std::sync::LazyLock;

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use hir::Semantic;

use crate::Assist;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
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
        static PATTERNS: LazyLock<Vec<(String, ())>> =
            LazyLock::new(|| vec![(format!("ssr: length(lists:flatten({LIST_ARG_VAR}))."), ())]);

        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<bool> {
        // A comment inside a placeholder is spliced verbatim into the fix, so
        // only decline when a comment sits in the surrounding syntax the fix
        // rewrites.
        if let Some(comments) = matched.comments_outside_placeholders(ctx.sema)
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
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let inefficient_call_range = matched.range.range;
        let nested_list_arg_match_src = matched.placeholder_text(ctx.sema, LIST_ARG_VAR)?;
        let mut builder = SourceChangeBuilder::new(ctx.file_id);
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

    #[test]
    fn fixes_inefficient_flatlength_preserving_comment_in_arg() {
        // The comment is inside the list argument (an SSR placeholder), which
        // is spliced verbatim into the fix, so it is preserved.
        check_fix(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn() -> len~gth(lists:flatten([a, % keep me
                                        b])).
            "#,
            expect![[r#"
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn() -> lists:flatlength([a, % keep me
                                        b]).
            "#]],
        )
    }

    #[test]
    fn ignores_inefficient_flatlength_with_comment_in_structure() {
        // The comment is attached to the `lists:flatten(...)` call the fix
        // discards, not to a placeholder, so it would be lost -- decline.
        check_diagnostics(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> length(lists:flatten( % keep me
                                                NestedList)).
            "#,
        )
    }
}
