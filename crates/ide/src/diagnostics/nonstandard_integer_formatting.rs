/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Formats integer literals for readability, e.g.:
//!
//! 1000000 becomes 1_000_000

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::AstNode;
use elp_text_edit::TextRange;
use hir::AnyExpr;
use hir::BasedInteger;
use hir::Expr;
use hir::FunctionDef;
use hir::Literal;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: true,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        integer_literal_format(diags, sema, file_id);
    },
};

fn integer_literal_format(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.for_each_function(file_id, |def| {
        check_function(diagnostics, sema, file_id, def)
    });
}

fn check_function(
    diagnostics: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    def: &FunctionDef,
) {
    let def_fb = def.in_function_body(sema, def);
    def_fb.clone().fold_function(
        Strategy {
            // Don't overwrite a macro with an inline literal
            macros: MacroStrategy::DoNotExpand,
            // Don't capture the text of any parens as part of the integer literal
            parens: ParenStrategy::VisibleParens,
        },
        (),
        &mut |_acc, clause_id, ctx: hir::fold::AnyCallBackCtx| {
            // N.B. negation is handled as a unary operator, so we don't need to handle that here
            let to_format = match &ctx.item {
                AnyExpr::Expr(Expr::Literal(Literal::Integer(i))) => Some(i),
                AnyExpr::Pat(Pat::Literal(Literal::Integer(i))) => Some(i),
                _ => None,
            };
            if let Some(integer_to_format) = to_format {
                let map = def_fb.get_body_map(clause_id);
                if let Some(src_ptr) = map.any(ctx.item_id) {
                    if src_ptr.file_id() != file_id {
                        // We've somehow ended up with a match in a different file - this means we've
                        // somehow expanded a macro from a different file, or some other complex case that
                        // gets hairy, so bail out.
                        return;
                    }
                    let db = sema.db.upcast();
                    let src_ast = src_ptr.to_ast(db);
                    let src_syntax = src_ast.syntax();
                    let src_text = src_syntax.text().to_string();
                    if let Some(formatted_src_text) = format_integer(integer_to_format) {
                        if formatted_src_text != src_text {
                            if let Some(diagnostic) = make_diagnostic(
                                sema,
                                file_id,
                                src_syntax.text_range(),
                                &formatted_src_text,
                            ) {
                                diagnostics.push(diagnostic);
                            }
                        }
                    }
                }
            }
        },
    )
}

fn format_integer(integer: &BasedInteger) -> Option<String> {
    match integer.base {
        2 => Some(format_base_two_integer(format!("{:b}", integer.value))),
        10 => Some(format_base_ten_integer(integer.value.to_string())),
        16 => Some(format_base_sixteen_integer(format!("{:X}", integer.value))),
        _ => None,
    }
}

fn format_base_ten_integer(magnitude_part: String) -> String {
    format_value_part(magnitude_part, 3)
}

fn format_base_sixteen_integer(magnitude_part: String) -> String {
    format!("16#{}", format_value_part(magnitude_part, 4))
}

fn format_base_two_integer(magnitude_part: String) -> String {
    format!("2#{}", format_value_part(magnitude_part, 4))
}

fn format_value_part(number: String, underscores_every: usize) -> String {
    number
        .replace("_", "") // Remove an existing underscores
        .to_uppercase() // For bases above 10, normalise to uppercase letters
        .as_bytes() // Safe to treat as bytes, since we're only operating within the ASCII range
        .rchunks(underscores_every) // Chunk into groups of digits to be separated (in reverse, so we're right aligned when we chunk the digits)
        .rev() // Reverse again so the chunks are in the correct order
        .map(std::str::from_utf8)
        .collect::<Result<Vec<&str>, _>>()
        .unwrap()
        .join("_") // Join the chunks with our numeric separator
}

fn make_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    integer_src_range: TextRange,
    formatted_integer: &str,
) -> Option<Diagnostic> {
    let message = "Non-standard integer formatting.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    builder.replace(integer_src_range, formatted_integer);
    let fix_msg: &str = if formatted_integer.len() < 16 {
        &format!("Reformat integer to {}", formatted_integer)
    } else {
        "Reformat integer"
    };
    let fixes = vec![fix(
        "fix_integer_formatting",
        fix_msg,
        builder.finish(),
        integer_src_range,
    )];
    Some(
        Diagnostic::new(
            DiagnosticCode::NonStandardIntegerFormatting,
            message,
            integer_src_range,
        )
        .experimental()
        .with_severity(Severity::Information)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes)),
    )
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;
    use crate::tests::check_diagnostics_with_config;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().set_experimental(true);
        check_diagnostics_with_config(config, fixture)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        let config = DiagnosticsConfig::default().set_experimental(true);
        tests::check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[test]
    fn ignores_well_formatted_integers() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 1}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 10}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 100}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 1_000}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 2#100}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 2#1001}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 2#1_1001}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 16#A}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 16#A1}.
         "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 16#F_BC1A}.
         "#,
        )
    }

    #[test]
    fn detects_no_underscores_base_10_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 10000}.
         %%            ^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_no_underscores_base_10_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 100~00}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 10_000}.
            "#]],
        )
    }

    #[test]
    fn detects_no_underscores_base_16_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 16#4865316F774F6C64}.
         %%            ^^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_no_underscores_base_16_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 16#4865316F774F6~C64}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 16#4865_316F_774F_6C64}.
            "#]],
        )
    }

    #[test]
    fn detects_no_underscores_base_2_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 2#1011011110010001}.
         %%            ^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_no_underscores_base_2_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 2#1011~011110010001}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 2#1011_0111_1001_0001}.
            "#]],
        )
    }

    #[test]
    fn detects_no_underscores_negative_base_10_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -10000}.
         %%             ^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_preceeding_zeroes() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 01~0}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 10}.
            "#]],
        );
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 00100~00}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 10_000}.
            "#]],
        );
    }

    #[test]
    fn fixes_no_underscores_negative_base_10_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -100~00}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, -10_000}.
            "#]],
        )
    }

    #[test]
    fn detects_no_underscores_negative_base_16_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -16#4865316F774F6C64}.
         %%             ^^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_no_underscores_negative_base_16_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -16#48653~16F774F6C64}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, -16#4865_316F_774F_6C64}.
            "#]],
        )
    }

    #[test]
    fn detects_no_underscores_negative_base_2_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -2#1011011110010001}.
         %%             ^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_no_underscores_negative_base_2_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -2#1011~011110010001}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, -2#1011_0111_1001_0001}.
            "#]],
        )
    }

    #[test]
    fn fixes_bad_underscores_base_10_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 100~0_0}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 10_000}.
            "#]],
        )
    }

    #[test]
    fn detects_bad_underscores_base_16_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 16#4865316F7_74F6_C64}.
         %%            ^^^^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_bad_underscores_base_16_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 16#48653~16F7_74F6_C64}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 16#4865_316F_774F_6C64}.
            "#]],
        )
    }

    #[test]
    fn detects_bad_underscores_base_2_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 2#1011011_110010001}.
         %%            ^^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_bad_underscores_base_2_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 2#1~011_011110010001}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, 2#1011_0111_1001_0001}.
            "#]],
        )
    }

    #[test]
    fn detects_bad_underscores_negative_base_10_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -1000_0}.
         %%             ^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_bad_underscores_negative_base_10_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -1_00~00}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, -10_000}.
            "#]],
        )
    }

    #[test]
    fn detects_bad_underscores_negative_base_16_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -16#4865316F7_74F6C64}.
         %%             ^^^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_bad_underscores_negative_base_16_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -16#48653_1_6F774F6~C64}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, -16#4865_316F_774F_6C64}.
            "#]],
        )
    }

    #[test]
    fn detects_bad_underscores_negative_base_2_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -2#101_1011110010001}.
         %%             ^^^^^^^^^^^^^^^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_bad_underscores_negative_base_2_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -2#1011~01111_001_0001}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, -2#1011_0111_1001_0001}.
            "#]],
        )
    }

    #[test]
    fn detects_explicit_positive_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, +10000}.
         %%             ^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_explicit_positive_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, +100~00}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, +10_000}.
            "#]],
        )
    }

    #[test]
    fn detects_when_there_is_a_space_between_sign_and_integer() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -  10000}.
         %%               ^^^^^ 💡 information: Non-standard integer formatting.
            "#,
        )
    }

    #[test]
    fn fixes_when_there_is_a_space_between_sign_and_integer() {
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -  100~00}.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> {val, -  10_000}.
            "#]],
        )
    }

    #[test]
    fn ignores_non_standard_bases() {
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 3#10222110012}.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 3#1022_2_110012}.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 4#10322110012}.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 4#10_3221100_12}.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, -22#10_3221100_12}.
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> {val, 36#1F0_3221100_12}.
            "#,
        )
    }

    #[test]
    fn handles_very_big_integers() {
        // Works for big integers
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> 1234567~89.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> 123_456_789.
            "#]],
        );
        // Works for very big integers
        check_fix(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> 12345678912345~6789.
            "#,
            expect![[r#"
         -module(integer_literal_format).

         fn() -> 123_456_789_123_456_789.
            "#]],
        );
        // Gives up (but doesn't crash!) for extremely big integers
        check_diagnostics(
            r#"
         //- /src/integer_literal_format.erl
         -module(integer_literal_format).

         fn() -> 123456789123456789123~456789123456789123456789.
            "#,
        )
    }
}
