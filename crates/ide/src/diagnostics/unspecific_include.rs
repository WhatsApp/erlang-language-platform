/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unspecific-include
//
// Return a warning if an include path is not fully specified

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::generated_file_include_lib;
use elp_ide_db::elp_base_db::path_for_file;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use elp_syntax::ast;
use hir::InFile;
use hir::Semantic;

use crate::Assist;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::fix;

// Assist: rewrite_include
//
// Rewrite a simple include attribute to have a full path.
//
// ```
// -include("some_header_from_app_a.hrl").
// ```
// ->
// ```
// -include("app_a/include/some_header_from_app_a.hrl").
// ```

pub(crate) struct UnspecificIncludeLinter;

impl Linter for UnspecificIncludeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnspecificInclude
    }

    fn description(&self) -> &'static str {
        "Unspecific include."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    replacement: String,
    make_include_lib: Option<TextRange>,
}

impl GenericLinter for UnspecificIncludeLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let form_list = sema.form_list(file_id);
        let source_file = sema.parse(file_id);
        let mut res = Vec::new();

        for (idx, inc) in form_list.includes() {
            if inc.path().contains("/") {
                continue;
            }

            let included_file_id = match sema.db.resolve_include(InFile::new(file_id, idx)) {
                Some(id) => id,
                None => continue,
            };

            if included_file_id == file_id {
                continue;
            }

            // Skip diagnostic if both files are in the same app
            let source_app_name = sema.db.file_app_name(file_id);
            let included_app_name = sema.db.file_app_name(included_file_id);
            if source_app_name.is_some() && source_app_name == included_app_name {
                continue;
            }

            let include_path = match path_for_file(sema.db.upcast(), included_file_id) {
                Some(p) => p,
                None => continue,
            };

            if include_path.to_string().contains("/src/") {
                continue;
            }

            let replacement = match generated_file_include_lib(
                sema.db.upcast(),
                file_id,
                included_file_id,
                include_path,
            ) {
                Some(r) => r,
                None => continue,
            };

            let ast = inc.form_id().get(&source_file.value);
            let range_and_lib = match ast {
                ast::Form::PreprocessorDirective(preprocessor_directive) => {
                    match preprocessor_directive {
                        ast::PreprocessorDirective::PpInclude(pp_include) => pp_include
                            .include_range()
                            .map(|r| (r, Some(pp_include.text_range()))),
                        ast::PreprocessorDirective::PpIncludeLib(pp_include_lib) => {
                            pp_include_lib.include_range().map(|r| (r, None))
                        }
                        _ => None,
                    }
                }
                _ => None,
            };

            if let Some((range, make_include_lib)) = range_and_lib {
                res.push(GenericLinterMatchContext {
                    range,
                    context: Context {
                        replacement,
                        make_include_lib,
                    },
                });
            }
        }

        Some(res)
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let mut builder = TextEdit::builder();
        let filename = &context.replacement;
        if let Some(attr_range) = context.make_include_lib {
            builder.replace(attr_range, format!("-include_lib(\"{filename}\")."));
        } else {
            builder.replace(range, format!("\"{filename}\""));
        }
        let edit = builder.finish();
        Some(vec![fix(
            "replace_unspecific_include",
            &format!("Replace include path with: {filename}"),
            SourceChange::from_text_edit(file_id, edit),
            range,
        )])
    }
}

pub static LINTER: UnspecificIncludeLinter = UnspecificIncludeLinter;

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;
    // @fb-only: use elp_ide_db::meta_only::MetaOnlyDiagnosticCode;
    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::UnspecificInclude
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[rustfmt::skip]
    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        let config = DiagnosticsConfig::default()
            // @fb-only: .disable(DiagnosticCode::MetaOnly(MetaOnlyDiagnosticCode::MalformedInclude))
            .disable(DiagnosticCode::UnusedInclude);
        tests::check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[test]
    fn detects_unspecific_include() {
        check_diagnostics(
            r#"
         //- /app_a/src/unspecific_include.erl app:app_a include_path:/app_b/include
           -module(unspecific_include).
           -include("some_header_from_app_a.hrl").
           %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0037: Unspecific include.

         //- /app_b/include/some_header_from_app_a.hrl include_path:/app_b/include app:app_b
           -define(A,3).
            "#,
        )
    }

    #[test]
    fn no_diagnostic_for_same_dir_include() {
        check_diagnostics(
            r#"
           //- /app_a/src/unspecific_include.erl app:app_a
           -module(unspecific_include).
           -include("some~_header_from_app_a.hrl").
           //- /app_a/src/some_header_from_app_a.hrl app:app_a include_path:/app_a/include
           -define(A,3)."#,
        )
    }

    #[test]
    fn fixes_unspecific_include() {
        check_fix(
            r#"
           //- /app_a/src/unspecific_include.erl include_path:/app_b/include app:app_a
           -module(unspecific_include).
           -include("some~_header_from_app_a.hrl").
           %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unspecific include.
           //- /app_b/include/some_header_from_app_a.hrl include_path:/app_b/include app:app_b
           -define(A,3)."#,
            // Note: the test fixture include path is not ideal for this, see lint_reports_bxl_project_error test in elp/main
            expect![[r#"
                -module(unspecific_include).
                -include_lib("app_b/include/some_header_from_app_a.hrl").
            "#]],
        )
    }

    #[test]
    fn fixes_unspecific_include_lib() {
        check_fix(
            r#"
           //- /app_a/src/unspecific_include.erl include_path:/app_b/include app:app_a
           -module(unspecific_include).
           -include_lib("some~_header_from_app_a.hrl").
           %%           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unspecific include.
           //- /app_b/include/some_header_from_app_a.hrl include_path:/app_b/include app:app_b
           -define(A,3)."#,
            // Note: the test fixture include path is not ideal for this, see lint_reports_bxl_project_error test in elp/main
            expect![[r#"
                -module(unspecific_include).
                -include_lib("app_b/include/some_header_from_app_a.hrl").
            "#]],
        )
    }
}
