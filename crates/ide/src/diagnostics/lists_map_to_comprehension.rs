/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: lists_map_to_comprehension
//!
//! warn on code of the form `lists:map(fun (X) -> Body end, L)` and suggest `[Body || X <:- L]`
//! Also handles `lists:map(fun action/1, L)`, `lists:map(fun mod:action/1, L)`,
//! and `lists:map(F, L)` where F is a variable.

use std::sync::LazyLock;

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_ssr::Match;
use elp_ide_ssr::is_placeholder_a_var_from_sema_and_match;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::Assist;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PatternKind {
    /// `lists:map(fun (X) -> Body end, List)`
    AnonymousFun,
    /// `lists:map(fun action/1, List)`
    LocalFunRef,
    /// `lists:map(fun mod:action/1, List)`
    RemoteFunRef,
    /// `lists:map(F, List)` where F is a variable
    FunVar,
}

pub(crate) struct ListsMapToComprehensionLinter;

impl Linter for ListsMapToComprehensionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::ListsMapToComprehension
    }

    fn description(&self) -> &'static str {
        "lists:map/2 call can be replaced with a list comprehension."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }
}

impl SsrPatternsLinter for ListsMapToComprehensionLinter {
    type Context = PatternKind;

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        static PATTERNS: LazyLock<Vec<(String, PatternKind)>> = LazyLock::new(|| {
            vec![
                (
                    format!("ssr: lists:map(fun ({ELEM_VAR}) -> {BODY_VAR} end, {LIST_VAR})."),
                    PatternKind::AnonymousFun,
                ),
                (
                    format!("ssr: lists:map(fun {FUN_VAR}/1, {LIST_VAR})."),
                    PatternKind::LocalFunRef,
                ),
                (
                    format!("ssr: lists:map(fun {MOD_VAR}:{FUN_VAR}/1, {LIST_VAR})."),
                    PatternKind::RemoteFunRef,
                ),
                (
                    format!("ssr: lists:map({FUN_VAR}, {LIST_VAR})."),
                    PatternKind::FunVar,
                ),
            ]
        });

        &PATTERNS
    }

    fn is_match_valid(
        &self,
        context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<bool> {
        // A comment inside a placeholder is spliced verbatim into the fix, so
        // only decline when a comment sits in the surrounding syntax the fix
        // rewrites away.
        if let Some(comments) = matched.comments_outside_placeholders(ctx.sema)
            && !comments.is_empty()
        {
            return None;
        }

        if *context == PatternKind::FunVar {
            let fun_placeholder = matched.get_placeholder_match(FUN_VAR)?;
            if !is_placeholder_a_var_from_sema_and_match(ctx.sema, matched, &fun_placeholder) {
                return None;
            }
        }

        Some(true)
    }

    fn fixes(
        &self,
        context: &Self::Context,
        matched: &Match,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let call_range = matched.range.range;
        let list = matched.placeholder_text(ctx.sema, LIST_VAR)?;

        let comprehension = match context {
            PatternKind::AnonymousFun => {
                let var = matched.placeholder_text(ctx.sema, ELEM_VAR)?;
                let body = matched.placeholder_text(ctx.sema, BODY_VAR)?;
                format!("[{body} || {var} <:- {list}]")
            }
            PatternKind::LocalFunRef => {
                let fun = matched.placeholder_text(ctx.sema, FUN_VAR)?;
                format!("[{fun}(Elem) || Elem <:- {list}]")
            }
            PatternKind::RemoteFunRef => {
                let module = matched.placeholder_text(ctx.sema, MOD_VAR)?;
                let fun = matched.placeholder_text(ctx.sema, FUN_VAR)?;
                format!("[{module}:{fun}(Elem) || Elem <:- {list}]")
            }
            PatternKind::FunVar => {
                let fun = matched.placeholder_text(ctx.sema, FUN_VAR)?;
                format!("[{fun}(Elem) || Elem <:- {list}]")
            }
        };

        let mut builder = SourceChangeBuilder::new(ctx.file_id);
        builder.replace(call_range, comprehension);
        let fixes = vec![fix(
            "lists_map_to_comprehension",
            "Rewrite lists:map/2 as a list comprehension",
            builder.finish(),
            call_range,
        )];
        Some(fixes)
    }
}

pub(crate) static LINTER: ListsMapToComprehensionLinter = ListsMapToComprehensionLinter;

static ELEM_VAR: &str = "_@Var";
static BODY_VAR: &str = "_@Body";
static LIST_VAR: &str = "_@List";
static FUN_VAR: &str = "_@Fun";
static MOD_VAR: &str = "_@Mod";

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::ListsMapToComprehension
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    #[test]
    fn detects_lists_map_with_fun() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun (X) -> X + 1 end, L).
         %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0066: lists:map/2 call can be replaced with a list comprehension.
            "#,
        )
    }

    #[test]
    fn fixes_lists_map_with_fun() {
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun (X) -> X +~ 1 end, L).
            "#,
            expect![[r#"
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> [X + 1 || X <:- L].
            "#]],
        )
    }

    #[test]
    fn fixes_lists_map_with_fun_closure() {
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(Y, L) -> lists:map(fun (X) -> X +~ Y end, L).
            "#,
            expect![[r#"
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(Y, L) -> [X + Y || X <:- L].
            "#]],
        )
    }

    #[test]
    fn fixes_lists_map_with_tuple_pattern() {
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun ({K, V}) -> {V,~ K} end, L).
            "#,
            expect![[r#"
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> [{V, K} || {K, V} <:- L].
            "#]],
        )
    }

    #[test]
    fn fixes_lists_map_with_complex_body() {
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun (X) -> 360 div erlang:a~bs(X) end, L).
            "#,
            expect![[r#"
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> [360 div erlang:abs(X) || X <:- L].
            "#]],
        )
    }

    #[test]
    fn ignores_funs_with_guards() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun (X) when X > 7 -> X + 1 end, L).
            "#,
        )
    }

    #[test]
    fn ignores_funs_with_multiple_clauses() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun ({X}) -> X + 1; ([W]) -> W + 2 end, L). % Unclear what name to give to the fun parameter before pattern matching on it, so ignore for now
            "#,
        )
    }

    #[test]
    fn detects_local_fun_ref() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun action/1, L).
         %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0066: lists:map/2 call can be replaced with a list comprehension.
            "#,
        )
    }

    #[test]
    fn fixes_local_fun_ref() {
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun acti~on/1, L).
            "#,
            expect![[r#"
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> [action(Elem) || Elem <:- L].
            "#]],
        )
    }

    #[test]
    fn detects_remote_fun_ref() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun mod:action/1, L).
         %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0066: lists:map/2 call can be replaced with a list comprehension.
            "#,
        )
    }

    #[test]
    fn fixes_remote_fun_ref() {
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun mod:acti~on/1, L).
            "#,
            expect![[r#"
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> [mod:action(Elem) || Elem <:- L].
            "#]],
        )
    }

    #[test]
    fn detects_fun_var() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(F, L) -> lists:map(F, L).
         %%          ^^^^^^^^^^^^^^^ 💡 weak: W0066: lists:map/2 call can be replaced with a list comprehension.
            "#,
        )
    }

    #[test]
    fn fixes_fun_var() {
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(F, L) -> lists:map(~F, L).
            "#,
            expect![[r#"
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(F, L) -> [F(Elem) || Elem <:- L].
            "#]],
        )
    }

    #[test]
    fn ignores_arity_not_one_local() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun action/2, L).
            "#,
        )
    }

    #[test]
    fn ignores_arity_not_one_remote() {
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map(fun mod:action/2, L).
            "#,
        )
    }

    #[test]
    fn fixes_lists_map_preserving_comment_in_body() {
        // The comment is inside the fun body (a placeholder), spliced verbatim
        // into the comprehension, so it is preserved.
        check_fix(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:m~ap(fun ({K, V}) -> {V, % keep me
                                             K} end, L).
         "#,
            expect![[r#"
                -module(lists_map_to_comprehension).

                % elp:ignore W0017 (undefined_function)
                fn(L) -> [{V, % keep me
                                                    K} || {K, V} <:- L].
   "#]],
        )
    }

    #[test]
    fn ignores_lists_map_with_comment_in_structure() {
        // The comment is attached to the `lists:map(...)` wrapper the fix
        // discards, outside every placeholder, so it would be lost -- decline.
        check_diagnostics(
            r#"
         //- /src/lists_map_to_comprehension.erl
         -module(lists_map_to_comprehension).

         % elp:ignore W0017 (undefined_function)
         fn(L) -> lists:map( % keep me
                            fun (X) -> X + 1 end, L).
            "#,
        )
    }
}
