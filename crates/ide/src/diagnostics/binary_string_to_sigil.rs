/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: binary_string_to_sigil
//!
//! Warn on code of the form `<<"bar">>` and suggest `~"bar"` (Erlang sigil syntax)

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::match_pattern_in_file_functions;
use elp_project_model::otp;
use hir::Semantic;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|acc, sema, file_id, _ext| {
        if otp::supports_eep66_sigils() {
            binary_string_to_sigil_ssr(acc, sema, file_id);
        }
    },
};

static STRING_CONTENT_VAR: &str = "_@StringContent";

fn binary_string_to_sigil_ssr(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let matches = match_pattern_in_file_functions(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!("ssr: <<{STRING_CONTENT_VAR}>>.").as_str(),
    );
    matches.matches.iter().for_each(|m| {
        if let Some(diagnostic) = make_diagnostic(sema, m) {
            diags.push(diagnostic);
        }
    });
}

fn make_diagnostic(sema: &Semantic, matched: &Match) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let binary_string_range = matched.range.range;
    let string_content_match_src = matched.placeholder_text(sema, STRING_CONTENT_VAR)?;

    // Only process if the content is a string literal (starts and ends with quotes)
    if !string_content_match_src.starts_with('"') || !string_content_match_src.ends_with('"') {
        return None;
    }

    let mut builder = SourceChangeBuilder::new(file_id);
    let sigil_string = format!("~{string_content_match_src}");
    builder.replace(binary_string_range, sigil_string);
    let fixes = vec![fix(
        "binary_string_to_sigil",
        "Convert to sigil syntax",
        builder.finish(),
        binary_string_range,
    )];

    let message = "Binary string can be written using sigil syntax.".to_string();
    Some(
        Diagnostic::new(
            DiagnosticCode::BinaryStringToSigil,
            message,
            binary_string_range,
        )
        .with_severity(Severity::WeakWarning)
        .with_ignore_fix(sema, file_id)
        .with_fixes(Some(fixes)),
    )
}

#[cfg(test)]
mod tests {

    use elp_project_model::otp;
    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::BinaryStringToSigil
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        if otp::supports_eep66_sigils() {
            tests::check_filtered_diagnostics(fixture, &filter)
        }
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        if otp::supports_eep66_sigils() {
            tests::check_fix(fixture_before, fixture_after)
        }
    }

    #[test]
    fn detects_binary_string() {
        check_diagnostics(
            r#"
         //- /src/binary_string_to_sigil.erl
         -module(binary_string_to_sigil).

         fn() -> <<"hello">>.
         %%      ^^^^^^^^^^^ 💡 weak: Binary string can be written using sigil syntax.
            "#,
        )
    }

    #[test]
    fn ignores_non_binary_string() {
        check_diagnostics(
            r#"
         //- /src/binary_string_to_sigil.erl
         -module(binary_string_to_sigil).

         fn(X) -> <<X>>.
            "#,
        )
    }

    #[test]
    fn ignores_binary_sigil_string() {
        check_diagnostics(
            r#"
         //- /src/binary_string_to_sigil.erl
         -module(binary_string_to_sigil).

         fn() ->
             ~"monkey ~2..0b\n",
             ~b"monkey ~2..0b\n".
            "#,
        )
    }

    #[test]
    fn fixes_binary_string_to_sigil() {
        check_fix(
            r#"
         //- /src/binary_string_to_sigil.erl
         -module(binary_string_to_sigil).

         fn() -> <<"he~llo">>.
            "#,
            expect![[r#"
         -module(binary_string_to_sigil).

         fn() -> ~"hello".
            "#]],
        )
    }

    #[test]
    fn fixes_binary_string_with_escapes() {
        check_fix(
            r#"
         //- /src/binary_string_to_sigil.erl
         -module(binary_string_to_sigil).

         fn() -> <<"hel~lo \"world\"">>.
            "#,
            expect![[r#"
         -module(binary_string_to_sigil).

         fn() -> ~"hello \"world\"".
            "#]],
        )
    }
}
