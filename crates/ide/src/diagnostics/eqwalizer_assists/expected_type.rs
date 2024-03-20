/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

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
use hir::FunctionBody;
use hir::InFile;
use hir::Literal;
use hir::Semantic;
use hir::SpecBody;
use hir::SpecId;
use hir::SpecSig;
use hir::TypeExpr;
use text_edit::TextEdit;
use text_edit::TextSize;

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
                    format!("Update returned value to '{expected}'").as_str(),
                    SourceChange::from_text_edit(file_id, edit),
                    d.range,
                ));
                add_spec_fix(sema, file_id, got, diagnostic);
            }

            // 2-Tuple with leading atom vs just the second part
            (Type::TupleType(TupleType { arg_tys }), other2) => {
                match &arg_tys[..] {
                    [atom @ Type::AtomLitType(AtomLitType { .. }), other] => {
                        if other == other2 {
                            // Add wrapping tuple to return
                            let file_text = sema.db.file_text(file_id);
                            let current = &file_text[d.range.start().into()..d.range.end().into()];
                            let replacement = format!("{{{atom}, {current}}}");
                            let edit = TextEdit::replace(d.range, replacement.clone());
                            diagnostic.add_fix(fix(
                                "fix_expected_type",
                                format!("Update returned value to '{replacement}'").as_str(),
                                SourceChange::from_text_edit(file_id, edit),
                                d.range,
                            ));

                            // Remove tuple from spec
                            add_spec_fix(sema, file_id, got, diagnostic);
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}

fn add_spec_fix(
    sema: &Semantic,
    file_id: FileId,
    got: &Type,
    diagnostic: &mut Diagnostic,
) -> Option<()> {
    let (spec_id, spec_body, _function_body) = get_spec(sema, file_id, diagnostic.range.start())?;
    match &spec_body.sigs[..] {
        [sig] => {
            match &spec_body.body[sig.result] {
                TypeExpr::Literal(Literal::Atom(_)) => {
                    // We have a single atom. Make an edit to replace it.
                    let (_, body_map) = sema.db.spec_body_with_source(spec_id);
                    let source = body_map.type_expr(sig.result)?;
                    let range = source.range();
                    let edit = TextEdit::replace(range, format!("{got}").to_string());
                    diagnostic.add_fix(fix(
                        "fix_expected_type",
                        format!("Update function spec to return '{got}'").as_str(),
                        SourceChange::from_text_edit(file_id, edit),
                        diagnostic.range,
                    ));
                }
                TypeExpr::Tuple { args } => match args[..] {
                    [_atom, _rest] => {
                        let fix_label = format!("Update function spec to return '{got}'");
                        make_spec_fix(sema, file_id, spec_id, sig, fix_label, got, diagnostic)?;
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        _ => {}
    }
    None
}

fn make_spec_fix(
    sema: &Semantic,
    file_id: FileId,
    spec_id: InFile<SpecId>,
    sig: &SpecSig,
    fix_label: String,
    got: &Type,
    diagnostic: &mut Diagnostic,
) -> Option<()> {
    let (_, body_map) = sema.db.spec_body_with_source(spec_id);
    let source = body_map.type_expr(sig.result)?;
    let range = source.range();
    let edit = TextEdit::replace(range, format!("{got}").to_string());
    diagnostic.add_fix(fix(
        "fix_expected_type",
        &fix_label.as_str(),
        SourceChange::from_text_edit(file_id, edit),
        diagnostic.range,
    ));
    Some(())
}

fn get_spec(
    sema: &Semantic,
    file_id: FileId,
    offset: TextSize,
) -> Option<(InFile<SpecId>, Arc<SpecBody>, Arc<FunctionBody>)> {
    let token = find_best_token(sema, FilePosition { file_id, offset })?;
    let function = sema.find_enclosing_function_body(file_id, &token.value.parent()?)?;
    let spec_body: Arc<SpecBody> = function.spec_body()?;
    let spec_id = InFile::new(file_id, spec_body.spec_id()?);
    Some((spec_id, spec_body, function))
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
            "Update returned value to 'spec_atom'",
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
            "Update function spec to return 'something_else'",
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
            "Update returned value to '{ok, 53}'",
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

    #[test]
    fn mismatched_tuple_fix_spec() {
        check_specific_fix(
            "Update function spec to return 'number()'",
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

            -spec baz() -> number().
            baz() -> 53.
         "#,
        )
    }
}
