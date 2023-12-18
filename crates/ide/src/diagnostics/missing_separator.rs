/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::ast::AstNode;
use elp_syntax::ast::ClauseSeparator;
use hir::Semantic;

use super::make_unexpected_diagnostic;
use crate::diagnostics::make_missing_diagnostic;
use crate::Diagnostic;

// Diagnostic: missing separator (W0004)
//
// Diagnostic for separators missing in function clauses.

// TODO: combine with head_mismatch?
pub(crate) fn missing_separator_semantic(
    diagnostics: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
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
                            diagnostics.push(make_missing_diagnostic(
                                range,
                                ";",
                                "missing_semi".to_string(),
                            ));
                        }

                        Some((ClauseSeparator::Semi, _range)) => {}
                        Some((ClauseSeparator::Dot, range)) => {
                            diagnostics.push(make_unexpected_diagnostic(
                                range,
                                ".",
                                "unexpected_dot".to_string(),
                            ));
                        }
                        None => {
                            let ast_fun = fun.form_id.get_ast(sema.db, file_id);
                            if let Some(last_tok) = ast_fun.syntax().last_token() {
                                let range = last_tok.text_range();
                                diagnostics.push(make_missing_diagnostic(
                                    range,
                                    ";",
                                    "missing_semi".to_string(),
                                ))
                            };
                        }
                    }
                } else {
                    match fun.separator {
                        Some((ClauseSeparator::Missing, range)) => {
                            diagnostics.push(make_missing_diagnostic(
                                range,
                                ".",
                                "missing_dot".to_string(),
                            ));
                        }

                        Some((ClauseSeparator::Semi, range)) => {
                            diagnostics.push(make_unexpected_diagnostic(
                                range,
                                ";",
                                "unexpected_semi".to_string(),
                            ));
                        }
                        Some((ClauseSeparator::Dot, _range)) => {}
                        None => {
                            let ast_fun = fun.form_id.get_ast(sema.db, file_id);
                            if let Some(last_tok) = ast_fun.syntax().last_token() {
                                let range = last_tok.text_range();
                                diagnostics.push(make_missing_diagnostic(
                                    range,
                                    ".",
                                    "missing_dot".to_string(),
                                ))
                            };
                        }
                    };
                }
            })
    });
}

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
        let text = concat!("foo(2)->3.");

        let parsed = ast::SourceFile::parse_text(text);
        let d = form_missing_separator_diagnostics(&parsed);
        assert_eq!(format!("{:?}", d), "[]")
    }

    #[test]
    fn fun_decl_missing_semi_no_warning_2() {
        let text = concat!("foo(1)->2;\n", "foo(2)->3.");

        let parsed = ast::SourceFile::parse_text(text);
        let d = form_missing_separator_diagnostics(&parsed);
        assert_eq!(format!("{:?}", d), "[]")
    }

    #[test]
    fn fun_decl_missing_semi_1() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2
        %% ^ warning: Missing ';'
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
        %% ^ warning: Missing ';'
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
        %% ^ warning: Missing '.'
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
         %% ^ warning: Unexpected '.'
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
         %% ^ warning: Unexpected ';'
"#,
        );
    }
}
