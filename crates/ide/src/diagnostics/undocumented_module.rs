/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::AstNode;
use hir::Semantic;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::UndocumentedModule;
const DIAGNOSTIC_MESSAGE: &str = "The module is not documented.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::WeakWarning;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false,
        default_disabled: true,
    },
    checker: &|diags, sema, file_id, _ext| {
        check(diags, sema, file_id);
    },
};

fn check(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) -> Option<()> {
    let def_map = sema.def_map_local(file_id);
    let module_attribute = sema.module_attribute(file_id)?;
    let module_has_no_docs = def_map.moduledoc.is_empty()
        && def_map.moduledoc_metadata.is_empty()
        && sema
            .module_edoc_header(file_id, &module_attribute)
            .is_none();
    if module_has_no_docs {
        let module_name = module_attribute.name()?;
        let module_name_range = module_name.syntax().text_range();
        let diagnostic = Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, module_name_range)
            .with_severity(DIAGNOSTIC_SEVERITY);
        diagnostics.push(diagnostic);
    }
    Some(())
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;

    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::UndocumentedModule);
        tests::check_diagnostics_with_config(config, fixture);
    }

    #[test]
    fn test_undocumented_module() {
        check_diagnostics(
            r#"
     -module(main).
          %% ^^^^ weak: The module is not documented.
         "#,
        )
    }

    #[test]
    fn test_module_with_moduledoc() {
        check_diagnostics(
            r#"
     -module(main).
     -moduledoc """
     This is a module
     """.
         "#,
        )
    }

    #[test]
    fn test_module_with_moduledoc_metadata() {
        check_diagnostics(
            r#"
     -module(main).
     -moduledoc #{deprecated => "Use something else."}.
         "#,
        )
    }

    #[test]
    fn test_module_with_moduledoc_and_moduledoc_metadata() {
        check_diagnostics(
            r#"
     -module(main).
     -moduledoc """
     This is a module
     """.
     -moduledoc #{deprecated => "Use something else."}.
         "#,
        )
    }

    #[test]
    fn test_module_with_deprecated_edoc_comments() {
        check_diagnostics(
            r#"
     % @doc
    %% ^^^^ 💡 weak: EDoc style comments are deprecated. Please use Markdown instead.
     % This is a module
     -module(main).
         "#,
        )
    }
}
