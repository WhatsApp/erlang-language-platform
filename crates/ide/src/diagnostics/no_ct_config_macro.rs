/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: no_ct_config_macro
//!
//! warn on the deprecated Common Test `?config(Key, Config)` macro and suggest
//! `proplists:get_value(Key, Config)` instead

use std::sync::LazyLock;

use elp_ide_assists::Assist;
use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::SsrSearchScope;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) struct NoCtConfigMacroLinter;

impl Linter for NoCtConfigMacroLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoCtConfigMacro
    }

    fn description(&self) -> &'static str {
        "The `?config/2` macro is deprecated; use `proplists:get_value/2` instead."
    }

    fn is_enabled(&self) -> bool {
        false
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        // `?config/2` only carries the Common Test meaning in a suite or test
        // helper; elsewhere a `?config` macro is something else entirely.
        Some(true) == sema.db.is_test_suite_or_test_helper(file_id)
    }
}

impl SsrPatternsLinter for NoCtConfigMacroLinter {
    type Context = ();

    fn strategy(&self) -> Strategy {
        Strategy {
            // `?config` is a macro; match the invocation rather than its expansion.
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn scope(&self, ctx: &LinterContext) -> SsrSearchScope {
        // A `?config` inside a macro definition body is reachable only by
        // folding the whole file, since `DoNotExpand` stops at a use site whose
        // macro body is a single expression. A body of several comma-separated
        // expressions is spliced into the use site instead, so each use yields
        // another match on the same definition range and the diagnostic
        // repeats. No such macro wraps `?config` in the tree, so that is left
        // as is rather than given a dedup pass in the shared SSR emit loop.
        SsrSearchScope::WholeFile(ctx.file_id)
    }

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        static PATTERNS: LazyLock<Vec<(String, ())>> =
            LazyLock::new(|| vec![(format!("ssr: ?config({KEY_VAR}, {CONFIG_VAR})."), ())]);

        &PATTERNS
    }

    fn fixes(
        &self,
        _context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let range = matched.range.range;
        // Rewrite the callee alone rather than the whole invocation. Nested
        // `?config(K1, ?config(K2, Config))` matches twice, and two whole-call
        // replacements overlap, which `SourceChange::insert_source_edit` rejects
        // when `--apply-fix` merges every fix for a file into one edit.
        let callee = callee_range(ctx, range)?;
        // Only a comment overlapping the callee is at risk: the argument list
        // is left alone, so a comment there survives the rewrite. A callee
        // comment would be rewritten away, and could also have supplied the
        // `(` that `callee_range` took for the argument list's.
        if let Some(comments) = matched.comments(ctx.sema)
            && comments.iter().any(|comment| {
                comment
                    .syntax()
                    .text_range()
                    .intersect(callee)
                    .is_some_and(|overlap| !overlap.is_empty())
            })
        {
            return None;
        }
        let mut builder = SourceChangeBuilder::new(ctx.file_id);
        builder.replace(callee, "proplists:get_value(");
        Some(vec![fix(
            "no_ct_config_macro",
            "Rewrite to use proplists:get_value/2",
            builder.finish(),
            range,
        )])
    }
}

/// The `?config(` prefix of a matched invocation: the `?` through the opening
/// parenthesis of the argument list, which is the first `(` in the match.
///
/// Derived from the matched range's own text, so the rewrite can never reach
/// outside the invocation. `matched` indexes `ctx.file_id` because the default
/// `SsrPatternsLinter::filter_match` drops matches from any other file, which
/// this linter does not override. A placeholder range cannot stand in for the
/// end of the callee: for a parameterized macro body such as
/// `-define(V(Key, C), ?config(Key, C), foo).` the match sits in the definition
/// while `_@Key` binds at the use site, so a range ending at the placeholder
/// would span, and delete, everything between the two.
fn callee_range(ctx: &LinterContext, matched: TextRange) -> Option<TextRange> {
    let file_text = ctx.sema.db.file_text(ctx.file_id);
    let text = file_text.text(ctx.sema.db);
    let start: usize = matched.start().into();
    let end: usize = matched.end().into();
    let open_paren = text.get(start..end)?.find('(')?;
    let callee_end = TextSize::new(u32::try_from(start + open_paren + 1).ok()?);
    Some(TextRange::new(matched.start(), callee_end))
}

pub(crate) static LINTER: NoCtConfigMacroLinter = NoCtConfigMacroLinter;

static KEY_VAR: &str = "_@Key";
static CONFIG_VAR: &str = "_@Config";

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::NoCtConfigMacro
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::NoCtConfigMacro);
        tests::check_filtered_diagnostics_with_config(config, &vec![], fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::NoCtConfigMacro);
        tests::check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[test]
    fn detects_config_macro() {
        check_diagnostics(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         t(Config) -> ?config(my_key, Config).
         %%           ^^^^^^^^^^^^^^^^^^^^^^^ warning: W0086: The `?config/2` macro is deprecated; use `proplists:get_value/2` instead.
         %%                                 | 💡 Rewrite to use proplists:get_value/2
         %%                                 | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn detects_config_macro_in_macro_definition() {
        check_diagnostics(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         -define(GET(C), ?config(my_key, C)).
         %%              ^^^^^^^^^^^^^^^^^^ warning: W0086: The `?config/2` macro is deprecated; use `proplists:get_value/2` instead.
         %%                               | 💡 Rewrite to use proplists:get_value/2
         %%                               | 💡 <suppression>

         t(Config) -> ?GET(Config).
            "#,
        )
    }

    #[test]
    fn fixes_config_macro() {
        check_fix(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         t(Config) -> ?con~fig(my_key, Config).
            "#,
            expect![[r#"
         -module(no_ct_config_macro_SUITE).

         t(Config) -> proplists:get_value(my_key, Config).
            "#]],
        )
    }

    #[test]
    fn fixes_config_macro_in_parameterized_macro() {
        // The match sits in the definition body while `_@Key` binds at the use
        // site, so a rewrite anchored on the placeholder would span into `t/1`
        // and delete everything between the two.
        check_fix(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         -define(VALUES(Key, C), ?con~fig(Key, C), foo).

         t(C) -> [?VALUES(key, C)].
            "#,
            expect![[r#"
         -module(no_ct_config_macro_SUITE).

         -define(VALUES(Key, C), proplists:get_value(Key, C), foo).

         t(C) -> [?VALUES(key, C)].
            "#]],
        )
    }

    #[test]
    fn ignores_proplists_get_value() {
        check_diagnostics(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         t(Config) -> proplists:get_value(my_key, Config).
            "#,
        )
    }

    #[test]
    fn keeps_comment_in_argument_list() {
        // The rewrite leaves the argument list alone, so a comment there is
        // carried over untouched and must not withhold the fix.
        check_fix(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         t(Config) -> ?con~fig(my_key, % keep me
                               Config).
            "#,
            expect![[r#"
         -module(no_ct_config_macro_SUITE).

         t(Config) -> proplists:get_value(my_key, % keep me
                               Config).
            "#]],
        )
    }

    #[test]
    fn keeps_comment_abutting_the_open_paren() {
        // The comment starts exactly where the rewritten callee ends, so it
        // survives. Pins the boundary: an overlap test that accepted an empty
        // intersection would withhold the fix here.
        check_fix(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         t(Config) -> ?con~fig(% keep me
                               my_key, Config).
            "#,
            expect![[r#"
         -module(no_ct_config_macro_SUITE).

         t(Config) -> proplists:get_value(% keep me
                               my_key, Config).
            "#]],
        )
    }

    #[test]
    fn offers_no_rewrite_when_comment_in_callee() {
        // The comment is rewritten away, and its `(` is the one `callee_range`
        // would otherwise take for the argument list's. The diagnostic still
        // fires, but suppression is the only fix left -- so that is what gets
        // applied here.
        check_fix(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         t(Config) -> ?con~fig  % pick (the) key
                      (my_key, Config).
            "#,
            expect![[r#"
         -module(no_ct_config_macro_SUITE).

         % elp:ignore W0086 (no_ct_config_macro)
         t(Config) -> ?config  % pick (the) key
                      (my_key, Config).
            "#]],
        )
    }

    #[test]
    fn fixes_nested_config_macro() {
        // The inner call sits inside the outer call's range, so a fix that
        // rewrote whole invocations would produce overlapping edits.
        check_fix(
            r#"
         //- /test/no_ct_config_macro_SUITE.erl extra:test
         -module(no_ct_config_macro_SUITE).

         t(Config) -> ?con~fig(name, ?config(tc_group_properties, Config)).
            "#,
            expect![[r#"
         -module(no_ct_config_macro_SUITE).

         t(Config) -> proplists:get_value(name, ?config(tc_group_properties, Config)).
            "#]],
        )
    }

    #[test]
    fn ignores_non_test_files() {
        check_diagnostics(
            r#"
         //- /src/no_ct_config_macro.erl
         -module(no_ct_config_macro).

         t(Config) -> ?config(my_key, Config).
            "#,
        )
    }
}
