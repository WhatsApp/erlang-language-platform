/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::EqwalizerDiagnostic;
use elp_types_db::eqwalizer::tc_diagnostics::ExpectedSubtype;
use elp_types_db::eqwalizer::tc_diagnostics::TypeError;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::eqwalizer::StructuredDiagnostic;
use hir::Semantic;
use text_edit::TextEdit;

use crate::diagnostics::Diagnostic;
use crate::fix;

pub fn expected_type(
    _sema: &Semantic,
    file_id: FileId,
    d: &EqwalizerDiagnostic,
    diagnostic: &mut Diagnostic,
) {
    if let Some(StructuredDiagnostic::TypeError(TypeError::ExpectedSubtype(ExpectedSubtype {
        location: _,
        expr: _,
        expected,
        got,
    }))) = &d.diagnostic
    {
        // Start with a trivial case, mismatched atoms, change the range we have.
        match (expected, got) {
            (Type::AtomLitType(_), Type::AtomLitType(_)) => {
                let edit = TextEdit::replace(d.range, format!("{expected}").to_string());
                diagnostic.add_fix(fix(
                    "fix_expected_type",
                    format!("Change returned atom to '{expected}'").as_str(),
                    SourceChange::from_text_edit(file_id, edit),
                    d.range,
                ));
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn mismatched_atom() {
        check_diagnostics(
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
                -module(bar).

                -spec baz() -> spec_atom.
                baz() -> something_else.
                     %%% ^^^^^^^^^^^^^^ 💡 error: eqwalizer: incompatible_types
            "#,
        )
    }

    #[test]
    fn mismatched_atom_fix() {
        check_fix(
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
            -module(bar).

            -spec baz() -> spec_atom.
            baz() -> somet~hing_else.
                  %% ^^^^^^^^^^^^^^ 💡 error: eqwalizer: incompatible_types
            "#,
            r#"
            -module(bar).

            -spec baz() -> spec_atom.
            baz() -> spec_atom.
         "#,
        )
    }
}
