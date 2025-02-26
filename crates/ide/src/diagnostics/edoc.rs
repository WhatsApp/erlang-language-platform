/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: edoc

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use hir::Semantic;
use text_edit::TextRange;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::OldEdocSyntax;
const DIAGNOSTIC_MESSAGE: &str = "EDoc style comments are deprecated. Please use Markdown instead.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::Warning;
const CONVERT_FIX_ID: &str = "convert_to_markdown";
const CONVERT_FIX_LABEL: &str = "Convert to Markdown";

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: true,
    },
    checker: &|diags, sema, file_id, _ext| {
        check(diags, sema, file_id);
    },
};

fn check(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    if let Some(comments) = sema.file_edoc_comments(file_id) {
        for (_form, header) in comments {
            if let Some(doc_tag) = header.doc().next() {
                for range in doc_tag.text_ranges() {
                    diagnostics.push(old_edoc_syntax_diagnostic(file_id, range));
                }
            }
        }
    }
}

fn old_edoc_syntax_diagnostic(file_id: FileId, range: TextRange) -> Diagnostic {
    Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, range)
        .with_severity(DIAGNOSTIC_SEVERITY)
        .with_fixes(Some(vec![fix_convert_to_markdown(file_id, range)]))
}

fn fix_convert_to_markdown(file_id: FileId, range: TextRange) -> Assist {
    let mut builder = SourceChangeBuilder::new(file_id);
    builder.replace(range, "todo");
    let source_change = builder.finish();
    crate::fix(CONVERT_FIX_ID, CONVERT_FIX_LABEL, source_change, range)
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;
    use expect_test::Expect;

    use crate::tests;
    use crate::DiagnosticsConfig;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default().enable(DiagnosticCode::OldEdocSyntax)
    }

    fn check_diagnostics(fixture: &str) {
        tests::check_diagnostics_with_config(config(), fixture);
    }

    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix_with_config(config(), fixture_before, fixture_after);
    }

    #[test]
    fn test_module_doc() {
        check_diagnostics(
            r#"
    %% Copyright (c) Meta Platforms, Inc. and affiliates.
    %%
    %% This is some license text.
    %%%-------------------------------------------------------------------
    %% @doc This is the module documentation.
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    %%      With some more text.
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    %%      And some more lines.
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    %% @end
    %%%-------------------------------------------------------------------
    %%% % @format
    -module(main).

    main() ->
      dep().

    dep() -> ok.
        "#,
        )
    }

    #[test]
    fn test_function_doc() {
        check_diagnostics(
            r#"
    -module(main).
    %% @doc This is the main function documentation.
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    main() ->
      dep().

    dep() -> ok.
        "#,
        )
    }

    #[test]
    fn test_function_doc_fix() {
        check_fix(
            r#"
    -module(main).
    %% @doc This is the main f~unction documentation.
    main() ->
      dep().

    dep() -> ok.
        "#,
            expect![[r#"
    -module(main).
    todo
    main() ->
      dep().

    dep() -> ok.
        "#]],
        )
    }
}
