/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: length_lists_flatten_to_lists_flatlength
//!
//! warn on code of the form `length(lists:flatten(L))` and suggest `lists:flatlength(L)`

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
        inefficient_flatlength_ssr(acc, sema, file_id);
    },
};

static LIST_ARG_VAR: &str = "_@List";

fn inefficient_flatlength_ssr(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let matches = match_pattern_in_file_functions(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!("ssr: length(lists:flatten({LIST_ARG_VAR})).").as_str(),
    );
    matches.matches.iter().for_each(|m| {
        if let Some(diagnostic) = make_diagnostic(sema, m) {
            diags.push(diagnostic);
        }
    });
}

fn make_diagnostic(sema: &Semantic, matched: &Match) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let inefficient_call_range = matched.range.range;
    let nested_list_arg_match_src = matched.placeholder_text(sema, LIST_ARG_VAR)?;
    let message = "Unnecessary intermediate flat-list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_flatlength = format!("lists:flatlength({nested_list_arg_match_src})");
    builder.replace(inefficient_call_range, efficient_flatlength);
    let fixes = vec![fix(
        "unnecessary_flattening_to_find_flat_length",
        "Rewrite to use lists:flatlength/1",
        builder.finish(),
        inefficient_call_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::UnnecessaryFlatteningToFindFlatLength,
            message,
            inefficient_call_range,
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
         %%                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary intermediate flat-list allocated.
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
