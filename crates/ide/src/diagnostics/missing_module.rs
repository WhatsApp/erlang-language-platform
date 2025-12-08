/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: missing-module
//
// Return a diagnostic if a module does not have a module definition

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::ast;
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;

pub(crate) struct MissingModuleLinter;

impl Linter for MissingModuleLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MissingModule
    }

    fn description(&self) -> &'static str {
        "no module definition"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> super::Severity {
        Severity::Error
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        let file_kind = sema.db.file_kind(file_id);
        file_kind.is_module()
    }
}

impl GenericLinter for MissingModuleLinter {
    type Context = ();

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let parse = sema.db.parse(file_id);
        let mut res = Vec::new();

        for form in parse.tree().forms() {
            match form {
                ast::Form::PreprocessorDirective(_) => {
                    continue; // skip any directives
                }
                ast::Form::FileAttribute(_) => {
                    continue; // skip
                }
                ast::Form::ModuleAttribute(_) => {
                    break;
                }
                other_form => {
                    let range = other_form.syntax().text_range();
                    res.push(GenericLinterMatchContext { range, context: () });
                    break;
                }
            }
        }
        Some(res)
    }
}

pub static LINTER: MissingModuleLinter = MissingModuleLinter;

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;

    #[test]
    fn fun_decl_module_decl_ok() {
        check_diagnostics(
            r#"
-file("main.erl",1).
-define(baz,4).
-module(main).
foo(2)->?baz.
"#,
        );
    }

    #[test]
    fn fun_decl_module_decl_missing() {
        check_diagnostics(
            r#"
  -file("foo.erl",1).
  -define(baz,4).
  foo(2)->?baz.
%%^^^^^^^^^^^^^💡 error: L1201: no module definition
"#,
        );
    }

    #[test]
    fn fun_decl_module_decl_missing_2() {
        check_diagnostics(
            r#"
  baz(1)->4.
%%^^^^^^^^^^💡 error: L1201: no module definition
  foo(2)->3.
"#,
        );
    }

    #[test]
    fn fun_decl_module_decl_after_preprocessor() {
        check_diagnostics(
            r#"
-ifndef(snmpm_net_if_mt).
-module(main).
-endif.
baz(1)->4.
"#,
        );
    }
}
