/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: encode_hex_with_case
//!
//! Detect `string:lowercase(binary:encode_hex(_@X))` and suggest `binary:encode_hex(_@X, lowercase)`

use elp_ide_assists::Assist;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_project_model::otp;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::diagnostics::Linter;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct EncodeHexWithCaseLinter;

impl Linter for EncodeHexWithCaseLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::EncodeHexWithCase
    }

    fn description(&self) -> &'static str {
        "Use `binary:encode_hex/2` with a case argument instead of wrapping with `string:lowercase/1`."
    }

    fn is_enabled(&self) -> bool {
        otp::supports_binary_encode_hex_with_case()
    }
}

impl SsrPatternsLinter for EncodeHexWithCaseLinter {
    type Context = ();

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> = vec![(
                format!("ssr: string:lowercase(binary:encode_hex({ARG_VAR}))."),
                ()
            ),];
        }
        &PATTERNS
    }

    fn is_match_valid(
        &self,
        _context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<bool> {
        if matched.range.file_id != file_id {
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
        let arg_match = matched.placeholder_text(sema, ARG_VAR)?;
        let mut builder = SourceChangeBuilder::new(matched.range.file_id);
        let replacement = format!("binary:encode_hex({arg_match}, lowercase)");
        let range = matched.range.range;
        builder.replace(range, replacement);
        let fixes = vec![fix(
            "encode_hex_with_case",
            "Use binary:encode_hex/2 with case argument",
            builder.finish(),
            range,
        )];
        Some(fixes)
    }
}

pub(crate) static LINTER: EncodeHexWithCaseLinter = EncodeHexWithCaseLinter;

static ARG_VAR: &str = "_@X";

#[cfg(test)]
mod tests {

    use elp_project_model::otp;
    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::EncodeHexWithCase
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        if otp::supports_binary_encode_hex_with_case() {
            tests::check_filtered_diagnostics(fixture, &filter)
        }
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        if otp::supports_binary_encode_hex_with_case() {
            tests::check_fix(fixture_before, fixture_after)
        }
    }

    #[test]
    fn detects_encode_hex_with_lowercase() {
        check_diagnostics(
            r#"
        //- /src/encode_hex_with_case.erl
        -module(encode_hex_with_case).

        hex(X) ->
            string:lowercase(binary:encode_hex(X)).
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0072: Use `binary:encode_hex/2` with a case argument instead of wrapping with `string:lowercase/1`.
           "#,
        )
    }

    #[test]
    fn detects_encode_hex_with_complex_arg() {
        check_diagnostics(
            r#"
        //- /src/encode_hex_with_case.erl
        -module(encode_hex_with_case).

        hex(Data) ->
            string:lowercase(binary:encode_hex(crypto:hash(sha256, Data))).
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0072: Use `binary:encode_hex/2` with a case argument instead of wrapping with `string:lowercase/1`.
           "#,
        )
    }

    #[test]
    fn ignores_encode_hex_with_case_arg() {
        check_diagnostics(
            r#"
        //- /src/encode_hex_with_case.erl
        -module(encode_hex_with_case).

        hex(X) ->
            binary:encode_hex(X, lowercase).
           "#,
        )
    }

    #[test]
    fn ignores_lowercase_of_other_call() {
        check_diagnostics(
            r#"
        //- /src/encode_hex_with_case.erl
        -module(encode_hex_with_case).

        lower(X) ->
            string:lowercase(X).
           "#,
        )
    }

    #[test]
    fn ignores_encode_hex_alone() {
        check_diagnostics(
            r#"
        //- /src/encode_hex_with_case.erl
        -module(encode_hex_with_case).

        hex(X) ->
            binary:encode_hex(X).
           "#,
        )
    }

    #[test]
    fn fixes_encode_hex_with_lowercase() {
        check_fix(
            r#"
        //- /src/encode_hex_with_case.erl
        -module(encode_hex_with_case).

        hex(X) ->
            string:low~ercase(binary:encode_hex(X)).
           "#,
            expect![[r#"
        -module(encode_hex_with_case).

        hex(X) ->
            binary:encode_hex(X, lowercase).
           "#]],
        )
    }

    // SSR can return matches with file_id from header files
    // when the pattern is used in a macro argument. Only process matches
    // that belong to the current file being analyzed.
    #[test]
    fn ignores_match_in_macro_arg_from_header_file() {
        check_diagnostics(
            r#"
        //- /assert/include/assert.hrl app:assert include_path:/assert/include
        %% Padding to ensure header file range exceeds source file length
        %% This triggers file_id mismatch when SSR matches in macro arg
        -define(assert(BoolExpr),
            case (BoolExpr) of
                true -> ok;
                _ -> erlang:error(assertion_failed)
            end).

        //- /src/test.erl app:test
        -module(test).
        -include_lib("assert/include/assert.hrl").
        f(X) -> ?assert(string:lowercase(binary:encode_hex(X))).
           "#,
        )
    }

    #[test]
    fn fixes_encode_hex_with_complex_arg() {
        check_fix(
            r#"
        //- /src/encode_hex_with_case.erl
        -module(encode_hex_with_case).

        hex(Data) ->
            string:low~ercase(binary:encode_hex(crypto:hash(sha256, Data))).
           "#,
            expect![[r#"
        -module(encode_hex_with_case).

        hex(Data) ->
            binary:encode_hex(crypto:hash(sha256, Data), lowercase).
           "#]],
        )
    }
}
