/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::find_best_token;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::EqwalizerDiagnostic;
use elp_types_db::eqwalizer::tc_diagnostics::ExpectedSubtype;
use elp_types_db::eqwalizer::tc_diagnostics::TypeError;
use elp_types_db::eqwalizer::types::AtomLitType;
use elp_types_db::eqwalizer::types::TupleType;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::eqwalizer::StructuredDiagnostic;
use hir::InFile;
use hir::Literal;
use hir::Semantic;
use hir::SpecBody;
use hir::TypeExpr;
use text_edit::TextEdit;

use crate::diagnostics::Diagnostic;
use crate::fix;

pub fn expected_type(
    sema: &Semantic,
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
        match (expected, got) {
            // mismatched atoms
            (Type::AtomLitType(_), Type::AtomLitType(_)) => {
                let edit = TextEdit::replace(d.range, format!("{expected}").to_string());
                diagnostic.add_fix(fix(
                    "fix_expected_type",
                    format!("Change returned atom to '{expected}'").as_str(),
                    SourceChange::from_text_edit(file_id, edit),
                    d.range,
                ));
                add_spec_fix_atoms(sema, file_id, got, diagnostic);
            }

            // 2-Tuple with leading atom vs just the second part
            (Type::TupleType(TupleType { arg_tys }), other2) => match &arg_tys[..] {
                [atom @ Type::AtomLitType(AtomLitType { .. }), other] => {
                    if other == other2 {
                        let file_text = sema.db.file_text(file_id);
                        let current = &file_text[d.range.start().into()..d.range.end().into()];
                        let replacement = format!("{{{atom}, {current}}}");
                        let edit = TextEdit::replace(d.range, replacement.clone());
                        diagnostic.add_fix(fix(
                            "fix_expected_type",
                            format!("Change returned value to '{replacement}'").as_str(),
                            SourceChange::from_text_edit(file_id, edit),
                            d.range,
                        ));
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }
}

fn add_spec_fix_atoms(
    sema: &Semantic,
    file_id: FileId,
    got: &Type,
    diagnostic: &mut Diagnostic,
) -> Option<()> {
    let token = find_best_token(
        sema,
        FilePosition {
            file_id,
            offset: diagnostic.range.start(),
        },
    )?;
    let function_id = sema.find_enclosing_function(file_id, &token.value.parent()?)?;
    let function = sema.db.function_body(InFile::new(file_id, function_id));
    let spec_body: &SpecBody = function.spec_body()?;
    let spec_id = InFile::new(file_id, spec_body.spec_id()?);
    // We are looking for a single spec signature with a single atom return type.
    match &spec_body.sigs[..] {
        [sig] => {
            match spec_body.body[sig.result] {
                TypeExpr::Literal(Literal::Atom(_)) => {
                    // We have a single atom. Make an edit to replace it.
                    let (_, body_map) = sema.db.spec_body_with_source(spec_id);
                    let source = body_map.type_expr(sig.result)?;
                    let range = source.range();
                    let edit = TextEdit::replace(range, format!("{got}").to_string());
                    diagnostic.add_fix(fix(
                        "fix_expected_type",
                        format!("Change spec atom to '{got}'").as_str(),
                        SourceChange::from_text_edit(file_id, edit),
                        diagnostic.range,
                    ));
                }
                _ => {}
            }
        }
        _ => {}
    }
    None
}

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;
    use crate::tests::check_specific_fix;

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
    fn mismatched_atom_fix_return() {
        check_specific_fix(
            "Change returned atom to 'spec_atom'",
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

    #[test]
    fn mismatched_atom_fix_spec() {
        check_specific_fix(
            "Change spec atom to 'something_else'",
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
            -module(bar).

            -spec baz() -> spec_atom.
            baz() -> somethin~g_else.
                  %% ^^^^^^^^^^^^^^ 💡 error: eqwalizer: incompatible_types
            "#,
            r#"
            -module(bar).

            -spec baz() -> something_else.
            baz() -> something_else.
         "#,
        )
    }

    #[test]
    fn mismatched_tuple_fix_return() {
        check_specific_fix(
            "Change returned value to '{ok, 53}'",
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
            -module(bar).

            -spec baz() -> {ok, number()}.
            baz() -> 5~3.
                  %% ^^^^^^^^^ 💡 error: eqwalizer: incompatible_types
            "#,
            r#"
            -module(bar).

            -spec baz() -> {ok, number()}.
            baz() -> {ok, 53}.
         "#,
        )
    }
}
