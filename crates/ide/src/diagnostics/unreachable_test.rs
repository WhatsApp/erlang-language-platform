/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unreachable-test (W0008)
//
// Error when a Common Test test function is exported but not reachable
// from the all/0 callback (directly or via groups).

use std::borrow::Cow;

use elp_ide_db::common_test::CommonTestInfo;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use hir::NameArity;
use hir::Semantic;

use crate::common_test;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::Severity;

pub(crate) struct UnreachableTestLinter;

#[derive(Clone, Debug)]
pub(crate) struct Context {
    name: NameArity,
}

impl Linter for UnreachableTestLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnreachableTest
    }

    fn description(&self) -> &'static str {
        "Unreachable test"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Error
    }

    fn runs_on_save_only(&self) -> bool {
        true
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id) == FileKind::TestModule
    }
}

impl GenericLinter for UnreachableTestLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let CommonTestInfo::Result { all, groups } = &*ctx.ct_info() else {
            return None;
        };

        let runnable_names =
            common_test::runnable_names(ctx.sema, ctx.file_id, all, groups).ok()?;
        let exported_test_ranges = common_test::exported_test_ranges(ctx.sema, ctx.file_id);

        let results: Vec<_> = exported_test_ranges
            .into_iter()
            .filter(|(name, _)| !runnable_names.contains(name))
            .map(|(name, range)| GenericLinterMatchContext {
                range: FileRange {
                    file_id: ctx.file_id,
                    range,
                },
                context: Context { name },
            })
            .collect();

        if results.is_empty() {
            None
        } else {
            Some(results)
        }
    }

    fn match_description(&self, context: &Context) -> Cow<'_, str> {
        Cow::Owned(format!("Unreachable test ({})", context.name))
    }
}

pub(crate) static LINTER: UnreachableTestLinter = UnreachableTestLinter;

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::IncludeCodeActionAssists;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_nth_fix;

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::CannotEvaluateCTCallbacks);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn test_unreachable_test() {
        check_diagnostics(
            r#"
//- /my_app/test/unreachable_1_SUITE.erl extra:test
   -module(unreachable_1_SUITE).
   -export([all/0]).
   -export([a/1, b/1]).
   all() -> [a].
   a(_Config) ->
     ok.
   b(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (b/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_duplicate_module_name() {
        // Only one of these files wins the module index. The other must still have its
        // own all/0 honoured, rather than every test it exports being reported.
        // W0045 is what should flag the duplication, so keep it out of the way here.
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::CannotEvaluateCTCallbacks)
            .disable(DiagnosticCode::DuplicateModule);
        check_diagnostics_with_config(
            config,
            r#"
//- /app_a/test/dup_SUITE.erl app:app_a extra:test
   -module(dup_SUITE).
   -export([all/0]).
   -export([a/1, b/1]).
   all() -> [a].
   a(_Config) ->
     ok.
   b(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (b/1)
     ok.
//- /app_b/test/dup_SUITE.erl app:app_b extra:test
   -module(dup_SUITE).
   -export([all/0]).
   -export([c/1, d/1]).
   all() -> [c].
   c(_Config) ->
     ok.
   d(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (d/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_init_end() {
        check_diagnostics(
            r#"
//- /my_app/test/unreachable_init_SUITE.erl extra:test
   -module(unreachable_init_SUITE).
   -export([all/0]).
   -export([init_per_suite/1, end_per_suite/1]).
   -export([a/1, b/1]).
   all() -> [a].
   init_per_suite(Config) -> Config.
   end_per_suite(_Config) -> ok.
   a(_Config) ->
     ok.
   b(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (b/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_dynamic_all() {
        check_diagnostics(
            r#"
//- /my_app/test/unreachable_dynamic_SUITE.erl extra:test
   -module(unreachable_dynamic_SUITE).
   -export([all/0]).
   -export([init_per_suite/1, end_per_suite/1]).
   -export([a/1, b/1]).
   all() -> do_all().
   do_all() -> [a].
   init_per_suite(Config) -> Config.
   end_per_suite(_Config) -> ok.
   a(_Config) ->
     ok.
   b(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (b/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_ignore() {
        check_diagnostics(
            r#"
//- /my_app/test/unreachable_ignore_SUITE.erl extra:test
   -module(unreachable_ignore_SUITE).
   -export([all/0]).
   -export([a/1, b/1, c/1]).
   all() -> [a].
   a(_Config) ->
     ok.
   % elp:ignore W0008
   b(_Config) ->
     ok.
   c(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (c/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_ignore_by_label() {
        check_diagnostics(
            r#"
//- /my_app/test/unreachable_ignore_label_SUITE.erl extra:test
   -module(unreachable_ignore_label_SUITE).
   -export([all/0]).
   -export([a/1, b/1, c/1]).
   all() -> [a].
   a(_Config) ->
     ok.
   % elp:ignore unreachable_test
   b(_Config) ->
     ok.
   c(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (c/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_fix() {
        check_diagnostics(
            r#"
//- /my_app/test/unreachable_fix1_SUITE.erl extra:test
    -module(unreachable_fix1_SUITE).
    -export([all/0]).
    -export([a/1, b/1, c/1]).
    all() -> [a].
    a(_Config) ->
      ok.
    b(_Config) ->
 %% ^ 💡 error: W0008: Unreachable test (b/1)
      ok.
    c(_Config) ->
 %% ^ 💡 error: W0008: Unreachable test (c/1)
      ok.
     "#,
        );
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::CannotEvaluateCTCallbacks);
        check_nth_fix(
            0,
            r#"
//- /my_app/test/unreachable_fix_SUITE.erl extra:test
-module(unreachable_fix_SUITE).
-export([all/0]).
-export([a/1, b/1, c/1]).
all() -> [a].
a(_Config) ->
  ok.
b(_Config) ->
  ok.
c~(_Config) ->
  ok.
     "#,
            expect![[r#"
-module(unreachable_fix_SUITE).
-export([all/0]).
-export([a/1, b/1, c/1]).
all() -> [a].
a(_Config) ->
  ok.
b(_Config) ->
  ok.
% elp:ignore W0008 (unreachable_test)
c(_Config) ->
  ok.
     "#]],
            config,
            &vec![],
            IncludeCodeActionAssists::No,
        );
    }

    #[test]
    fn test_unreachable_test_with_callback() {
        check_diagnostics(
            r#"
//- /my_app/test/unreachable_SUITE.erl extra:test
   -module(unreachable_SUITE).
   -export([all/0]).
   -export([a/1, b/1, my_callback/1]).
   -behaviour(my_behaviour).
   my_callback(X) -> X.
   all() -> [a].
   a(_Config) ->
     ok.
   b(_Config) ->
%% ^ 💡 error: W0008: Unreachable test (b/1)
     ok.
//- /my_app/src/my_behaviour.erl
-module(my_behaviour).
-export([foo/1]).
foo(X) -> X.
-callback my_callback(integer()) -> integer().
            "#,
        );
    }
}
