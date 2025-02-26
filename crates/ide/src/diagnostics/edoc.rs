/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: edoc

use elp_ide_db::elp_base_db::FileId;
use hir::Semantic;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: true,
    },
    checker: &|diags, sema, file_id, _ext| {
        edoc(diags, sema, file_id);
    },
};

fn edoc(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    if let Some(comments) = sema.file_edoc_comments(file_id) {
        for (_form, header) in comments {
            if let Some(doc) = header.doc().next() {
                for range in doc.text_ranges() {
                    diagnostics.push(
                        Diagnostic::new(DiagnosticCode::OldEdocSyntax, "Old EDoc syntax", range)
                            .with_severity(Severity::Warning),
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;

    use crate::tests::check_diagnostics_with_config;
    use crate::DiagnosticsConfig;

    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::OldEdocSyntax);
        check_diagnostics_with_config(config, fixture);
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
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Old EDoc syntax
    %%      With some more text.
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Old EDoc syntax
    %%      And some more lines.
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Old EDoc syntax
    %% @end
    %%%-------------------------------------------------------------------
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Old EDoc syntax
    %%% % @format
    %%<^^^^^^^^^^ warning: Old EDoc syntax
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
    %%<^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Old EDoc syntax
    main() ->
      dep().

    dep() -> ok.
        "#,
        )
    }
}
