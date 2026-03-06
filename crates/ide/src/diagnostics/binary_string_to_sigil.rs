/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: binary_string_to_sigil
//!
//! Warn on code of the form `<<"bar">>` and suggest `~"bar"` (Erlang sigil syntax)

use elp_ide_assists::Assist;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_project_model::otp;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use lazy_static::lazy_static;

use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct BinaryStringToSigilLinter;

impl Linter for BinaryStringToSigilLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::BinaryStringToSigil
    }

    fn description(&self) -> &'static str {
        "Binary string can be written using sigil syntax."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }

    fn is_enabled(&self) -> bool {
        otp::supports_eep66_sigils()
    }
}

impl SsrPatternsLinter for BinaryStringToSigilLinter {
    type Context = ();

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> =
                vec![(format!("ssr: <<{STRING_CONTENT_VAR}>>."), ()),];
        }
        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<bool> {
        let string_content_match_src = matched.placeholder_text(sema, STRING_CONTENT_VAR)?;
        // Only process if the content is a string literal (starts and ends with quotes)
        if !string_content_match_src.starts_with('"') || !string_content_match_src.ends_with('"') {
            return None;
        }
        // Ignore multi-line binary strings
        if string_content_match_src.contains('\n') {
            return None;
        }
        Some(true)
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let string_content_match_src = matched.placeholder_text(sema, STRING_CONTENT_VAR)?;
        let mut builder = SourceChangeBuilder::new(matched.range.file_id);
        let sigil_string = format!("~{string_content_match_src}");
        let binary_string_range = matched.range.range;
        builder.replace(binary_string_range, sigil_string);
        let fixes = vec![fix(
            "binary_string_to_sigil",
            "Convert to sigil syntax",
            builder.finish(),
            binary_string_range,
        )];
        Some(fixes)
    }

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }
}

pub(crate) static LINTER: BinaryStringToSigilLinter = BinaryStringToSigilLinter;

static STRING_CONTENT_VAR: &str = "_@StringContent";

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
         %%      ^^^^^^^^^^^ 💡 weak: W0051: Binary string can be written using sigil syntax.
            "#,
        )
    }

    #[test]
    fn ignores_binary_string_in_macro() {
        check_diagnostics(
            r#"
         //- /src/binary_string_to_sigil.erl
         -module(binary_string_to_sigil).

         -define(HELLO, <<"hello">>).
         fn() -> ?HELLO.
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
    fn ignores_multi_line_binary_string() {
        check_diagnostics(
            r#"
         //- /src/binary_string_to_sigil.erl
         -module(binary_string_to_sigil).

         fn() -> <<"Hello everyone! "
                   "Nice to meet you?">>.
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
