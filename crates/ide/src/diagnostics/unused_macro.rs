/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unused-macro
//
// Return a warning if a macro defined in an .erl file has no references to it

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_assists::helpers::extend_range;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;

pub(crate) struct UnusedMacroLinter;

impl Linter for UnusedMacroLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedMacro
    }
    fn description(&self) -> &'static str {
        "Unused macro."
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id).is_module()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    delete_range: TextRange,
    name: String,
}

impl GenericLinter for UnusedMacroLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut res = Vec::new();
        let def_map = sema.def_map_local(file_id);
        for (name, def) in def_map.get_macros() {
            if !SymbolDefinition::Define(def.clone())
                .usages(sema)
                .at_least_one()
                && let Some(range) = def.name_range(sema.db)
            {
                let context = Context {
                    delete_range: extend_range(def.source(sema.db).syntax()),
                    name: name.to_string(),
                };
                res.push(GenericLinterMatchContext {
                    range: FileRange { file_id, range },
                    context,
                });
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("Unused macro ({})", context.name))
    }

    fn tag(&self, _context: &Self::Context) -> Option<super::DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }

    fn fixes(
        &self,
        context: &Context,
        _range: TextRange,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let file_id = ctx.file_id;
        Some(vec![delete_unused_macro(
            file_id,
            context.delete_range,
            &context.name,
        )])
    }
}

pub static LINTER: UnusedMacroLinter = UnusedMacroLinter;

fn delete_unused_macro(file_id: FileId, range: TextRange, name: &str) -> Assist {
    let mut builder = TextEdit::builder();
    builder.delete(range);
    let edit = builder.finish();
    fix(
        "delete_unused_macro",
        &format!("Delete unused macro ({name})"),
        SourceChange::from_text_edit(file_id, edit),
        range,
    )
}

#[cfg(test)]
mod tests {

    use elp_ide_db::RootDatabase;
    use elp_ide_db::elp_base_db::RootQueryDb;
    use elp_ide_db::elp_base_db::assert_eq_expected;
    use elp_ide_db::elp_base_db::fixture::WithFixture;
    use expect_test::expect;

    use crate::AnalysisHost;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::fixture;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;
    use crate::tests::convert_diagnostics_to_annotations;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::HirUnresolvedMacro);
        check_diagnostics_with_config(config, fixture)
    }

    #[track_caller]
    pub(crate) fn check_diagnostics_ifdef_disabled(elp_fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::HirUnresolvedMacro);
        let (mut db, fixture) = RootDatabase::with_fixture(elp_fixture);
        db.set_ifdef_enabled(false);
        let host = AnalysisHost { db };
        let analysis = host.analysis();
        for file_id in &fixture.files {
            let file_id = *file_id;
            let diagnostics = fixture::diagnostics_for(
                &analysis,
                file_id,
                &config,
                &vec![],
                &fixture.diagnostics_enabled,
            );
            let diagnostics = diagnostics.diagnostics_for(file_id);

            let expected = fixture.annotations_by_file_id(&file_id);
            let actual = convert_diagnostics_to_annotations(diagnostics);
            assert_eq_expected!(expected, actual);
        }
    }

    #[test]
    fn test_unused_macro() {
        check_diagnostics(
            r#"
-module(main).
-define(MEANING_OF_LIFE, 42).
    %%  ^^^^^^^^^^^^^^^ 💡 warning: W0002: Unused macro (MEANING_OF_LIFE)
            "#,
        );
        check_fix(
            r#"
-module(main).
-define(MEA~NING_OF_LIFE, 42).
            "#,
            expect![[r#"
-module(main).
            "#]],
        )
    }

    #[test]
    fn test_unused_macro_not_applicable() {
        check_diagnostics(
            r#"
-module(main).
-define(MEANING_OF_LIFE, 42).
main() ->
  ?MEANING_OF_LIFE.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_not_applicable_for_hrl_file() {
        check_diagnostics(
            r#"
//- /include/foo.hrl
-define(MEANING_OF_LIFE, 42).
            "#,
        );
    }

    #[test]
    fn test_unused_macro_with_arg() {
        check_diagnostics(
            r#"
-module(main).
-define(USED_MACRO, used_macro).
-define(UNUSED_MACRO, unused_macro).
     %% ^^^^^^^^^^^^ 💡 warning: W0002: Unused macro (UNUSED_MACRO)
-define(UNUSED_MACRO_WITH_ARG(C), C).
     %% ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0002: Unused macro (UNUSED_MACRO_WITH_ARG/1)

main() ->
  ?MOD:foo(),
  ?USED_MACRO.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_dynamic_call() {
        // Ported from issue #1021 in Erlang LS
        check_diagnostics(
            r#"
-module(main).
-define(MOD, module). %% MOD
main() ->
  ?MOD:foo().
            "#,
        );
    }

    #[test]
    fn test_unused_macro_ifdef() {
        check_diagnostics(
            r#"
-module(main).
-define(GUARD, true).
-ifdef(GUARD).
main() -> ok.
-endif.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_ifndef() {
        check_diagnostics(
            r#"
-module(main).
-define(GUARD, true).
-ifndef(GUARD).
main() -> ok.
-endif.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_undef() {
        check_diagnostics(
            r#"
-module(main).
-define(TEMP, 42).
-undef(TEMP).
            "#,
        );
    }

    #[test]
    fn test_unused_macro_arity_overload() {
        check_diagnostics(
            r#"
-module(main).
-define(FOO, foo_no_args).
-define(FOO(X), X).
     %% ^^^ 💡 warning: W0002: Unused macro (FOO/1)
main() ->
  ?FOO.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_arity_overload_across_include() {
        // FOO/0 is defined in the module, FOO/1 in an included header.
        // The only call site is `?FOO(x)`, which resolves to FOO/1 from
        // the header — so FOO/0 in the module should be flagged unused.
        // Regression test: a bare-name match for "FOO" in macro_usages
        // is ambiguous when arity overloads come from included headers,
        // so the preprocessor fast path must fall through to body-level
        // DefineId resolution rather than declaring FOO/0 used.
        check_diagnostics(
            r#"
//- /src/main.hrl
-define(FOO(X), {foo, X}).
//- /src/main.erl
-module(main).
-include("main.hrl").
-define(FOO, foo_no_args).
     %% ^^^ 💡 warning: W0002: Unused macro (FOO)
main() ->
  ?FOO(x).
            "#,
        );
    }

    #[test]
    fn test_unused_macro_arity_overload_across_ifdef_else() {
        // Both -ifdef(TEST) and -else. branches define FOO/0 and FOO/1.
        // TEST is externally defined, so the -ifdef(TEST) branch is active.
        // The call sites exercise both arities and should resolve to the
        // active-branch defines — neither should be flagged as unused.
        check_diagnostics(
            r#"
//- /src/main.erl macros:[TEST]
-module(main).
-ifdef(TEST).
-define(FOO(), test_foo_0).
-define(FOO(X), X).
-else.
-define(FOO(), prod_foo_0).
-define(FOO(X), {prod, X}).
-endif.
main() ->
  ?FOO(),
  ?FOO(42).
            "#,
        );
    }

    #[test]
    fn test_unused_macro_include() {
        check_diagnostics(
            r#"
//- /src/foo.hrl
-define(A, a).
-define(B, b).
//- /src/foo.erl
-module(foo).
-include("foo.hrl").
-define(BAR, 42).
     %% ^^^ 💡 warning: W0002: Unused macro (BAR)
main() ->
  ?A.
        "#,
        );
    }

    #[test]
    fn test_unused_macro_ifdef_disabled() {
        check_diagnostics_ifdef_disabled(
            r#"
-module(main).
-define(MEANING_OF_LIFE, 42).
    %%  ^^^^^^^^^^^^^^^ 💡 warning: W0002: Unused macro (MEANING_OF_LIFE)
            "#,
        );
    }

    #[test]
    fn test_unused_macro_ifdef_disabled_not_applicable() {
        check_diagnostics_ifdef_disabled(
            r#"
-module(main).
-define(MEANING_OF_LIFE, 42).
main() ->
  ?MEANING_OF_LIFE.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_ifdef_disabled_with_ifdef() {
        check_diagnostics_ifdef_disabled(
            r#"
-module(main).
-define(GUARD, true).
-ifdef(GUARD).
main() -> ok.
-endif.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_ifdef_disabled_include() {
        check_diagnostics_ifdef_disabled(
            r#"
//- /src/foo.hrl
-define(A, a).
-define(B, b).
//- /src/foo.erl
-module(foo).
-include("foo.hrl").
-define(BAR, 42).
     %% ^^^ 💡 warning: W0002: Unused macro (BAR)
main() ->
  ?A.
        "#,
        );
    }
}
