/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_syntax::match_ast;
use hir::Semantic;
use lazy_static::lazy_static;
use regex::Regex;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::NoNoWarnSuppressions;
const DIAGNOSTIC_MESSAGE: &str = "Do not suppress compiler warnings at module level.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::Warning;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        no_warn_suppression(diags, sema, file_id);
    },
};

fn no_warn_suppression(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    lazy_static! {
        static ref NOWARN_REGEX: Regex = Regex::new(r"^nowarn_").unwrap();
    }

    let form_list = sema.db.file_form_list(file_id);
    for (_compile_option_idx, compile_option) in form_list.compile_attributes() {
        let attr = compile_option.form_id.get_ast(sema.db, file_id);
        if let Some(expr) = attr.options() {
            // Blindly search for any atom matching the nowarn_ prefix
            for n in expr.syntax().descendants() {
                match_ast! {
                    match n {
                        ast::Atom(atom) => {
                            if let Some(atom_text) = atom.text()
                                && NOWARN_REGEX.is_match(&atom_text) {
                                    let diagnostic = Diagnostic::new(
                                        DIAGNOSTIC_CODE,
                                        DIAGNOSTIC_MESSAGE,
                                        atom.syntax().text_range(),
                                    )
                                    .with_ignore_fix(sema, file_id)
                                    .with_severity(DIAGNOSTIC_SEVERITY);
                                    diagnostics.push(diagnostic);
                                }
                        },
                        _ => {}
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests;

    #[test]
    fn test_nowarn_atom() {
        tests::check_diagnostics(
            r#"
  -module(main).
  -compile(nowarn_export_all).
  %%       ^^^^^^^^^^^^^^^^^ 💡 warning: Do not suppress compiler warnings at module level.
              "#,
        )
    }

    #[test]
    fn test_nowarn_tuple() {
        tests::check_diagnostics(
            r#"
  -module(main).
  -compile({nowarn_unused_function, {unused_function, 1}}).
  %%        ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Do not suppress compiler warnings at module level.
              "#,
        )
    }

    #[test]
    fn test_nowarn_list() {
        tests::check_diagnostics(
            r#"
  -module(main).
  -compile([
      nowarn_export_all,
  %%  ^^^^^^^^^^^^^^^^^ 💡 warning: Do not suppress compiler warnings at module level.
      {nowarn_unused_function, {unused_function, 1}}
  %%   ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Do not suppress compiler warnings at module level.
  ]).
              "#,
        )
    }
}
