/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::path_for_file;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::IncludeCtx;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::DiagnosticCode;
use elp_syntax::ast;
use hir::InFile;
use hir::Semantic;
use text_edit::TextEdit;
use text_edit::TextRange;

use super::Diagnostic;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
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

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _file_kind| {
        check_includes(diags, sema, file_id);
    },
};

fn check_includes(acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let form_list = sema.form_list(file_id);

    form_list
        .includes()
        .filter(|(_, inc)| !inc.path().contains("/"))
        .filter_map(|(idx, inc)| {
            let included_file_id = sema.db.resolve_include(InFile::new(file_id, idx))?;
            Some((included_file_id, inc))
        })
        .filter(|(included_file_id, _)| *included_file_id != file_id)
        .for_each(|(included_file_id, inc)| {
            || -> Option<()> {
                let include_path = path_for_file(sema.db.upcast(), included_file_id)?;
                let app_data = sema.db.file_app_data(file_id)?;
                let candidate = include_path
                    .as_path()?
                    .strip_prefix(app_data.dir.as_path().parent()?)?;

                let ctx = &IncludeCtx::new(sema.db.upcast(), file_id);
                let resolved_file_id = ctx.resolve_include(candidate.as_str())?;
                let replacement = if resolved_file_id == included_file_id {
                    // We have an equivalent include
                    Some(candidate.as_str())
                } else {
                    None
                }?;
                let source_file = sema.parse(file_id);
                let ast = inc.form_id().get(&source_file.value);
                let range = match ast {
                    ast::Form::PreprocessorDirective(preprocessor_directive) => {
                        match preprocessor_directive {
                            ast::PreprocessorDirective::PpInclude(pp_include) => {
                                pp_include.include_range()
                            }
                            ast::PreprocessorDirective::PpIncludeLib(pp_include_lib) => {
                                pp_include_lib.include_range()
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }?;
                acc.push(make_diagnostic(file_id, range, replacement)?);
                Some(())
            }();
        });
}

fn make_diagnostic(file_id: FileId, range: TextRange, new_include: &str) -> Option<Diagnostic> {
    let message = format!("Unspecific include.");
    Some(
        Diagnostic::new(DiagnosticCode::UnspecificInclude, message, range)
            .with_severity(Severity::WeakWarning)
            .with_fixes(Some(vec![replace_include_path(
                file_id,
                range,
                new_include,
            )])),
    )
}

fn replace_include_path(file_id: FileId, range: TextRange, filename: &str) -> Assist {
    let mut builder = TextEdit::builder();
    builder.replace(range, format!("\"{}\"", filename.to_string()));
    let edit = builder.finish();
    fix(
        "replace_unspecific_include",
        &format!("Replace include path with: {filename}"),
        SourceChange::from_text_edit(file_id, edit),
        range,
    )
}

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;
    use expect_test::Expect;

    use crate::diagnostics::Diagnostic;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::UnspecificInclude
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
    fn detects_unspecific_include() {
        check_diagnostics(
            r#"
         //- /app_a/src/unspecific_include.erl
           -module(unspecific_include).
           -include("some_header_from_app_a.hrl").
           %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unspecific include.

         //- /app_a/include/some_header_from_app_a.hrl include_path:/app_a/include
           -define(A,3).
         
            "#,
        )
    }

    #[test]
    fn fixes_unspecific_include() {
        check_fix(
            r#"
           //- /app_a/src/unspecific_include.erl
           -module(unspecific_include).
           -include("some~_header_from_app_a.hrl").
           %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: Unspecific include.
           //- /app_a/include/some_header_from_app_a.hrl include_path:/app_a/include
           -define(A,3)."#,
            expect![[r#"
                    -module(unspecific_include).
                    -include("app_a/include/some_header_from_app_a.hrl").
            "#]],
        )
    }
}
