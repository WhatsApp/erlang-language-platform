/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: module-mismatch
//
// Diagnostic for mismatches between the module attribute name and the path of the given file

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::fix;

#[derive(Clone, Debug)]
pub(crate) struct ModuleMismatchContext {
    module_name: String,
    filename: String,
}

pub(crate) struct ModuleMismatchLinter;

impl Linter for ModuleMismatchLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::ModuleMismatch
    }

    fn description(&self) -> &'static str {
        "Module name does not match file name"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Error
    }
}

impl GenericLinter for ModuleMismatchLinter {
    type Context = ModuleMismatchContext;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let root_id = sema.db.file_source_root(file_id);
        let root = sema.db.source_root(root_id);
        let path = root.path_for_file(&file_id)?;
        let (filename, _) = path.name_and_extension()?;

        let module_name_ast = sema.module_attribute_name(file_id)?;
        let module_name_text = module_name_ast.text()?;

        if module_name_text.as_str() != filename {
            let range = module_name_ast.syntax().text_range();
            Some(vec![GenericLinterMatchContext {
                range: FileRange { file_id, range },
                context: ModuleMismatchContext {
                    module_name: module_name_text.to_string(),
                    filename: filename.to_string(),
                },
            }])
        } else {
            Some(vec![])
        }
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Module name ({}) does not match file name ({})",
            context.module_name, context.filename
        ))
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let mut builder = TextEdit::builder();
        builder.replace(range, context.filename.clone());
        let edit = builder.finish();
        Some(vec![fix(
            "rename_module_to_match_filename",
            &format!("Rename module to: {}", context.filename),
            SourceChange::from_text_edit(file_id, edit),
            range,
        )])
    }
}

pub static LINTER: ModuleMismatchLinter = ModuleMismatchLinter;

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_module_mismatch() {
        check_diagnostics(
            r#"
//- /src/foo.erl
-module(bar).
%%      ^^^ 💡 error: W0001: Module name (bar) does not match file name (foo)
"#,
        );
        check_fix(
            r#"
//- /src/foo.erl
-module(b~ar).
"#,
            expect![[r#"
-module(foo).
"#]],
        )
    }

    #[test]
    fn test_module_mismatch_correct() {
        check_diagnostics(
            r#"
//- /src/foo.erl
-module(foo).
            "#,
        );
    }
}
