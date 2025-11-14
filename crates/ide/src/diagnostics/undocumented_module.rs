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
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) struct UndocumentedModuleLinter;

impl Linter for UndocumentedModuleLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UndocumentedModule
    }

    fn description(&self) -> &'static str {
        "The module is not documented."
    }

    fn severity(&self) -> Severity {
        Severity::WeakWarning
    }

    fn is_enabled(&self) -> bool {
        false
    }

    fn should_process_test_files(&self) -> bool {
        false
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    module_name_range: TextRange,
}

impl GenericLinter for UndocumentedModuleLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let mut res = Vec::new();
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
            let context = Context { module_name_range };
            res.push(GenericLinterMatchContext {
                range: module_name_range,
                context,
            });
        }
        Some(res)
    }

    fn fixes(&self, context: &Context, sema: &Semantic, file_id: FileId) -> Option<Vec<Assist>> {
        let insert_offset = helpers::moduledoc_insert_offset(sema, file_id)?;
        let mut builder = SourceChangeBuilder::new(file_id);
        builder.insert(insert_offset, "-moduledoc false.\n");
        let source_change = builder.finish();
        let fix = fix(
            "add_moduledoc_false",
            "Add `-moduledoc false.` attribute",
            source_change,
            context.module_name_range,
        );
        Some(vec![fix])
    }
}

pub static LINTER: UndocumentedModuleLinter = UndocumentedModuleLinter;

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
