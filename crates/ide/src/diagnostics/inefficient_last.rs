/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: list_head_reverse_to_last
//!
//! warn on code of the form `hd(lists:reverse(L))` and suggest `lists:last(L)`

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::DiagnosticCode;
use elp_ide_ssr::match_pattern_in_file;
use elp_ide_ssr::Match;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;
use hir::Semantic;

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
        inefficient_last_hd_ssr(acc, sema, file_id);
        inefficient_last_pat_ssr(acc, sema, file_id);
    },
};

static LIST_VAR: &str = "_@List";
static LAST_ELEM_VAR: &str = "_@LastElem";

fn inefficient_last_hd_ssr(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let matches = match_pattern_in_file(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!("ssr: hd(lists:reverse({LIST_VAR})).").as_str(),
    );
    matches.matches.iter().for_each(|m| {
        let diagnostic = make_diagnostic_hd(sema, m);
        diags.push(diagnostic)
    });
}

fn inefficient_last_pat_ssr(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let matches = match_pattern_in_file(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!("ssr: [{LAST_ELEM_VAR}|_] = lists:reverse({LIST_VAR}).").as_str(),
    );
    matches.matches.iter().for_each(|m| {
        let diagnostic = make_diagnostic_pat(sema, m);
        diags.push(diagnostic)
    });
}

fn make_diagnostic_hd(sema: &Semantic, matched: &Match) -> Diagnostic {
    let file_id = matched.range.file_id;
    let inefficient_call_range = matched.range.range;
    let list_arg = matched.placeholder_text(sema, LIST_VAR).unwrap();
    let message = "Unnecessary intermediate reverse list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_last = format!("lists:last({list_arg})");
    builder.replace(inefficient_call_range, efficient_last);
    let fixes = vec![fix(
        "list_head_reverse_to_last",
        "Rewrite to use lists:last/1",
        builder.finish(),
        inefficient_call_range,
    )];
    Diagnostic::new(
        DiagnosticCode::ExpressionCanBeOptimised,
        message,
        inefficient_call_range,
    )
    .with_severity(Severity::Warning)
    .with_ignore_fix(sema, file_id)
    .with_fixes(Some(fixes))
}

fn make_diagnostic_pat(sema: &Semantic, matched: &Match) -> Diagnostic {
    let file_id = matched.range.file_id;
    let inefficient_call_range = matched.range.range;
    let list_arg = matched.placeholder_text(sema, LIST_VAR).unwrap();
    let last_elem_binding = matched.placeholder_text(sema, LAST_ELEM_VAR).unwrap();
    let message = "Unnecessary intermediate reverse list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_last = format!("{last_elem_binding} = lists:last({list_arg})");
    builder.replace(inefficient_call_range, efficient_last);
    let fixes = vec![fix(
        "list_head_reverse_to_last",
        "Rewrite to use lists:last/1",
        builder.finish(),
        inefficient_call_range,
    )];
    Diagnostic::new(
        DiagnosticCode::ExpressionCanBeOptimised,
        message,
        inefficient_call_range,
    )
    .with_severity(Severity::Warning)
    .with_ignore_fix(sema, file_id)
    .with_fixes(Some(fixes))
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
    fn detects_inefficient_last_via_hd() {
        check_diagnostics(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> hd(lists:reverse(List)).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unnecessary intermediate reverse list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_last_via_hd() {
        check_fix(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> hd(lists:re~verse(List)).
            "#,
            expect![[r#"
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:last(List).
            "#]],
        )
    }

    #[test]
    fn detects_inefficient_last_via_pattern() {
        check_diagnostics(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> [LastElem|_] = lists:reverse(List), LastElem.
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unnecessary intermediate reverse list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_last_via_pattern() {
        check_fix(
            r#"
         //- /src/inefficient_last.erl
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> [LastElem|_] = lists:re~verse(List), LastElem.
            "#,
            expect![[r#"
         -module(inefficient_last).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> LastElem = lists:last(List), LastElem.
            "#]],
        )
    }
}
