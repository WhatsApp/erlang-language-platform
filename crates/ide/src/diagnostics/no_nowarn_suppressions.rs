/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_syntax::match_ast;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct NoNoWarnSuppressionsLinter;

impl Linter for NoNoWarnSuppressionsLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoNoWarnSuppressions
    }

    fn description(&self) -> &'static str {
        "Do not suppress compiler warnings at module level."
    }
}

impl GenericLinter for NoNoWarnSuppressionsLinter {
    type Context = ();

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut res = Vec::new();
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
                                    && atom_text.starts_with("nowarn_") {
                                        let range = atom.syntax().text_range();
                                        res.push(GenericLinterMatchContext { range: FileRange { file_id, range }, context: () });
                                    }
                            },
                            _ => {}
                        }
                    }
                }
            }
        }
        Some(res)
    }
}

pub static LINTER: NoNoWarnSuppressionsLinter = NoNoWarnSuppressionsLinter;

#[cfg(test)]
mod tests {
    use crate::tests;

    #[test]
    fn test_nowarn_atom() {
        tests::check_diagnostics(
            r#"
  -module(main).
  -compile(nowarn_export_all).
  %%       ^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
  %%                       | 💡 <suppression>
              "#,
        )
    }

    #[test]
    fn test_nowarn_tuple() {
        tests::check_diagnostics(
            r#"
  -module(main).
  -compile({nowarn_unused_function, {unused_function, 1}}).
  %%        ^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
  %%                             | 💡 <suppression>
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
  %%  ^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
  %%                  | 💡 <suppression>
      {nowarn_unused_function, {unused_function, 1}}
  %%   ^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
  %%                        | 💡 <suppression>
  ]).
              "#,
        )
    }
}
