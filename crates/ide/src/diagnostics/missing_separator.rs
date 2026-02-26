/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: missing separator (W0004)
//
// Diagnostic for separators missing in function clauses.

// TODO: combine with head_mismatch?

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_syntax::ast::AstNode;
use elp_syntax::ast::ClauseSeparator;
use hir::Semantic;

use crate::Diagnostic;
use crate::diagnostics::GenericDiagnostics;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;

pub(crate) struct MissingSeparatorLinter;

impl Linter for MissingSeparatorLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::Missing("missing_separator".to_string())
    }

    fn description(&self) -> &'static str {
        "Missing or unexpected separator in function clauses."
    }

    fn can_be_suppressed(&self) -> bool {
        false
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }
}

impl GenericDiagnostics for MissingSeparatorLinter {
    fn diagnostics(
        &self,
        sema: &Semantic,
        file_id: FileId,
        severity: Severity,
        cli_severity: Severity,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let def_map = sema.def_map(file_id);

        def_map.get_functions().for_each(|(_, fun_def)| {
            let n = fun_def.function_clauses.len();
            fun_def
                .function_clauses
                .iter()
                .enumerate()
                .for_each(|(i, fun)| {
                    if i + 1 != n {
                        match fun.separator {
                            Some((ClauseSeparator::Missing, range)) => {
                                let d = Diagnostic::new(
                                    DiagnosticCode::Missing("missing_semi".to_string()),
                                    "Missing ';'",
                                    range,
                                )
                                .with_severity(severity)
                                .with_cli_severity(cli_severity);
                                diagnostics.push(d);
                            }
                            Some((ClauseSeparator::Semi, _range)) => {}
                            Some((ClauseSeparator::Dot, range)) => {
                                let d = Diagnostic::new(
                                    DiagnosticCode::Unexpected("unexpected_dot".to_string()),
                                    "Unexpected '.'",
                                    range,
                                )
                                .with_severity(severity)
                                .with_cli_severity(cli_severity);
                                diagnostics.push(d);
                            }
                            None => {
                                let ast_fun = fun.form_id.get_ast(sema.db, file_id);
                                if let Some(last_tok) = ast_fun.syntax().last_token() {
                                    let range = last_tok.text_range();
                                    let d = Diagnostic::new(
                                        DiagnosticCode::Missing("missing_semi".to_string()),
                                        "Missing ';'",
                                        range,
                                    )
                                    .with_severity(severity)
                                    .with_cli_severity(cli_severity);
                                    diagnostics.push(d);
                                }
                            }
                        }
                    } else {
                        match fun.separator {
                            Some((ClauseSeparator::Missing, range)) => {
                                let d = Diagnostic::new(
                                    DiagnosticCode::Missing("missing_dot".to_string()),
                                    "Missing '.'",
                                    range,
                                )
                                .with_severity(severity)
                                .with_cli_severity(cli_severity);
                                diagnostics.push(d);
                            }
                            Some((ClauseSeparator::Semi, range)) => {
                                let d = Diagnostic::new(
                                    DiagnosticCode::Unexpected("unexpected_semi".to_string()),
                                    "Unexpected ';'",
                                    range,
                                )
                                .with_severity(severity)
                                .with_cli_severity(cli_severity);
                                diagnostics.push(d);
                            }
                            Some((ClauseSeparator::Dot, _range)) => {}
                            None => {
                                let ast_fun = fun.form_id.get_ast(sema.db, file_id);
                                if let Some(last_tok) = ast_fun.syntax().last_token() {
                                    let range = last_tok.text_range();
                                    let d = Diagnostic::new(
                                        DiagnosticCode::Missing("missing_dot".to_string()),
                                        "Missing '.'",
                                        range,
                                    )
                                    .with_severity(severity)
                                    .with_cli_severity(cli_severity);
                                    diagnostics.push(d);
                                }
                            }
                        }
                    }
                })
        });

        diagnostics
    }
}

pub(crate) static LINTER: MissingSeparatorLinter = MissingSeparatorLinter;

// To run the tests via cargo
// cargo test --package elp_ide --lib
#[cfg(test)]
mod tests {
    use elp_syntax::ast;

    use crate::diagnostics::form_missing_separator_diagnostics;
    use crate::tests::check_diagnostics;
    // use crate::tests::check_fix;

    // The followings tests exercise missing separator for function directly.

    #[test]
    fn fun_decl_missing_semi_no_warning() {
        let text = "foo(2)->3.";

        let parsed = ast::SourceFile::parse_text(text);
        let d = form_missing_separator_diagnostics(&parsed);
        assert_eq!(format!("{d:?}"), "[]")
    }

    #[test]
    fn fun_decl_missing_semi_no_warning_2() {
        let text = concat!("foo(1)->2;\n", "foo(2)->3.");

        let parsed = ast::SourceFile::parse_text(text);
        let d = form_missing_separator_diagnostics(&parsed);
        assert_eq!(format!("{d:?}"), "[]")
    }

    #[test]
    fn fun_decl_missing_semi_1() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2
        %% ^ warning: W0004: Missing ';'
   foo(2)->3.
"#,
        );
    }

    #[test]
    fn fun_decl_missing_semi_2() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2;
   foo(2)->3
        %% ^ warning: W0004: Missing ';'
   foo(3)->4.
"#,
        );
    }

    #[test]
    fn fun_decl_missing_dot_1() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2;
   foo(2)->3
        %% ^ warning: W0004: Missing '.'
"#,
        );
    }

    #[test]
    fn fun_decl_misplaced_dot() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2;
   foo(2)->3.
         %% ^ warning: W0018: Unexpected '.'
   foo(3)->4.
"#,
        );
    }

    #[test]
    fn fun_decl_misplaced_semi() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2;
   foo(2)->3;
         %% ^ warning: W0018: Unexpected ';'
"#,
        );
    }
}
