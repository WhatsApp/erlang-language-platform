/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: lists_zip_lists_seq_to_lists_enumerate
//!
//! warn on code of the form `lists:zip(lists:seq(1,length(L)),L)` and suggest `lists:enumerate(L)`

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::SubId;
use elp_ide_ssr::match_pattern_in_file_functions;
use hir::AnyExprId;
use hir::BasedInteger;
use hir::Expr;
use hir::Literal;
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
        inefficient_enumerate_custom_index_ssr(acc, sema, file_id);
        inefficient_enumerate_custom_index_and_step_ssr(acc, sema, file_id);
    },
};

static LIST_VAR: &str = "_@List";
static INDEX_VAR: &str = "_@Index";
static STEP_VAR: &str = "_@Step";

fn inefficient_enumerate_custom_index_ssr(
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
        format!("ssr: lists:zip(lists:seq({INDEX_VAR},length({LIST_VAR})),{LIST_VAR}).").as_str(),
    );
    matches.matches.iter().for_each(|m| {
        if is_indexing_from_literal_one(sema, m) {
            if let Some(diagnostic) = make_diagnostic(sema, m) {
                diags.push(diagnostic);
            }
        } else if let Some(diagnostic) = make_diagnostic_custom_index(sema, m) {
            diags.push(diagnostic);
        }
    });
}

fn is_indexing_from_literal_one(sema: &Semantic, m: &Match) -> bool {
    || -> Option<bool> {
        let index_match = m.get_placeholder_match(sema, INDEX_VAR)?;
        let body = &m.matched_node_body.get_body(sema)?;
        match index_match.code_id {
            SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match body[expr_id] {
                Expr::Literal(Literal::Integer(BasedInteger { base: _, value: 1 })) => Some(true),
                _ => Some(false),
            },
            _ => Some(false),
        }
    }()
    .unwrap_or(false)
}

fn inefficient_enumerate_custom_index_and_step_ssr(
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
        format!("ssr: lists:zip(lists:seq({INDEX_VAR},{STEP_VAR},length({LIST_VAR})),{LIST_VAR}).")
            .as_str(),
    );
    matches.matches.iter().for_each(|m| {
        if let Some(diagnostic) = make_diagnostic_custom_index_and_step(sema, m) {
            diags.push(diagnostic);
        }
    });
}

fn make_diagnostic(sema: &Semantic, matched: &Match) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let inefficient_call_range = matched.range.range;
    let list_arg_matches = matched.placeholder_texts(sema, LIST_VAR)?;
    let list_arg = list_arg_matches.first()?;
    let message = "Unnecessary intermediate list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_enumerate = format!("lists:enumerate({list_arg})");
    builder.replace(inefficient_call_range, efficient_enumerate);
    let fixes = vec![fix(
        "lists_zip_lists_seq_to_lists_enumerate",
        "Rewrite to use lists:enumerate/1",
        builder.finish(),
        inefficient_call_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::ListsZipWithSeqRatherThanEnumerate,
            message,
            inefficient_call_range,
        )
        .with_severity(Severity::WeakWarning)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes)),
    )
}

fn make_diagnostic_custom_index(sema: &Semantic, matched: &Match) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let inefficient_call_range = matched.range.range;
    let list_arg_matches = matched.placeholder_texts(sema, LIST_VAR)?;
    let list_arg = list_arg_matches.first()?;
    let index_arg = matched.placeholder_text(sema, INDEX_VAR)?;
    let message = "Unnecessary intermediate list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_enumerate = format!("lists:enumerate({index_arg}, {list_arg})");
    builder.replace(inefficient_call_range, efficient_enumerate);
    let fixes = vec![fix(
        "lists_zip_lists_seq_to_lists_enumerate",
        "Rewrite to use lists:enumerate/2",
        builder.finish(),
        inefficient_call_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::ListsZipWithSeqRatherThanEnumerate,
            message,
            inefficient_call_range,
        )
        .with_severity(Severity::WeakWarning)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes)),
    )
}

fn make_diagnostic_custom_index_and_step(sema: &Semantic, matched: &Match) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let inefficient_call_range = matched.range.range;
    let list_arg_matches = matched.placeholder_texts(sema, LIST_VAR)?;
    let list_arg = list_arg_matches.first()?;
    let index_arg = matched.placeholder_text(sema, INDEX_VAR)?;
    let step_arg = matched.placeholder_text(sema, STEP_VAR)?;
    let message = "Unnecessary intermediate list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_enumerate = format!("lists:enumerate({index_arg}, {step_arg}, {list_arg})");
    builder.replace(inefficient_call_range, efficient_enumerate);
    let fixes = vec![fix(
        "list_zip_with_seq_rather_than_enumerate",
        "Rewrite to use lists:enumerate/3",
        builder.finish(),
        inefficient_call_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::ListsZipWithSeqRatherThanEnumerate,
            message,
            inefficient_call_range,
        )
        .with_severity(Severity::WeakWarning)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes))
        .add_categories([Category::SimplificationRule]),
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
        d.code == DiagnosticCode::ListsZipWithSeqRatherThanEnumerate
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
    fn detects_inefficient_enumerate() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(List) -> lists:zip(lists:seq(1,length(List)),List).
         %%          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn ignores_when_list_params_do_not_match() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(ListX,ListY) -> lists:zip(lists:seq(1,length(ListX)),ListY).
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_enumerate_over_length_call() {
        check_fix(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:zip(lists:s~eq(1, length(List)), List).
            "#,
            expect![[r#"
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(List) -> lists:enumerate(List).
            "#]],
        )
    }

    #[test]
    fn detects_inefficient_enumerate_custom_index() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(N, List) -> lists:zip(lists:seq(N,length(List)),List).
         %%             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_enumerate_over_length_call_custom_index() {
        check_fix(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, List) -> lists:zip(lists:s~eq(N, length(List)), List).
            "#,
            expect![[r#"
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, List) -> lists:enumerate(N, List).
            "#]],
        )
    }

    #[test]
    fn detects_inefficient_enumerate_custom_index_and_step() {
        check_diagnostics(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         fn(N, Step, List) -> lists:zip(lists:seq(N,Step,length(List)),List).
         %%                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unnecessary intermediate list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_enumerate_over_length_call_custom_index_and_step() {
        check_fix(
            r#"
         //- /src/inefficient_enumerate.erl
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, Step, List) -> lists:zip(lists:s~eq(N, Step, length(List)), List).
            "#,
            expect![[r#"
         -module(inefficient_enumerate).

         % elp:ignore W0017 (undefined_function)
         fn(N, Step, List) -> lists:enumerate(N, Step, List).
            "#]],
        )
    }
}
