/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_assists::Assist;
use elp_ide_assists::helpers;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::AstNode;
use elp_text_edit::TextRange;
use elp_text_edit::TextSize;
use hir::Semantic;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::UndocumentedModule;
const DIAGNOSTIC_MESSAGE: &str = "The module is not documented.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::WeakWarning;
const FIX_ID: &str = "add_moduledoc_false";
const FIX_LABEL: &str = "Add `-moduledoc false.` attribute";

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
        let moduledoc_insert_offset = helpers::moduledoc_insert_offset(sema, file_id)?;
        let fixes = fixes(file_id, moduledoc_insert_offset, module_name_range);
        let diagnostic = Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, module_name_range)
            .with_severity(DIAGNOSTIC_SEVERITY)
            .with_fixes(Some(fixes));
        diagnostics.push(diagnostic);
    }
    Some(())
}

fn fixes(file_id: FileId, insert_offset: TextSize, show_range: TextRange) -> Vec<Assist> {
    let mut builder = SourceChangeBuilder::new(file_id);
    builder.insert(insert_offset, "-moduledoc false.\n");
    let source_change = builder.finish();
    let fix = crate::fix(FIX_ID, FIX_LABEL, source_change, show_range);
    vec![fix]
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default().enable(DiagnosticCode::UndocumentedModule)
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_diagnostics_with_config(config(), fixture);
    }

    fn check_fix(before: &str, after: Expect) {
        tests::check_fix_with_config(config(), before, after);
    }

    #[test]
    fn test_undocumented_module() {
        check_diagnostics(
            r#"
     -module(main).
          %% ^^^^ 💡 weak: W0046: The module is not documented.
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
    %% ^^^^ 💡 warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
     % This is a module
     -module(main).
         "#,
        )
    }

    #[test]
    fn test_undocumented_module_fix() {
        check_fix(
            r#"
-module(ma~in).
"#,
            expect![[r#"
                -module(main).
                -moduledoc false.
            "#]],
        )
    }

    #[test]
    fn test_undocumented_module_fix_significant_attribute() {
        check_fix(
            r#"
-module(ma~in).
-oncall("my_oncall").
"#,
            expect![[r#"
                -module(main).
                -oncall("my_oncall").
                -moduledoc false.
            "#]],
        )
    }

    #[test]
    fn test_undocumented_module_fix_significant_attribute_2() {
        check_fix(
            r#"
-module(ma~in).
-oncall("my_oncall").

-ignore_xref([]).
"#,
            expect![[r#"
                -module(main).
                -oncall("my_oncall").
                -moduledoc false.

                -ignore_xref([]).
            "#]],
        )
    }

    #[test]
    fn test_undocumented_module_fix_significant_attribute_3() {
        check_fix(
            r#"
-module(ma~in).
-oncall("my_oncall").
-compile(warn_missing_spec_all).

-ignore_xref([]).
"#,
            expect![[r#"
                -module(main).
                -oncall("my_oncall").
                -compile(warn_missing_spec_all).
                -moduledoc false.

                -ignore_xref([]).
            "#]],
        )
    }
}
