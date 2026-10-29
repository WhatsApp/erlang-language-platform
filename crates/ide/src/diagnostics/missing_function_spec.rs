/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::sync::LazyLock;

use elp_ide_db::common_test::CommonTestInfo;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use fxhash::FxHashSet;
use hir::NameArity;
use hir::Semantic;
use hir::known;

use crate::common_test;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::Severity;

pub(crate) struct MissingFunctionSpec;

#[derive(Clone, Debug)]
pub(crate) struct Context {
    name: NameArity,
}

impl Linter for MissingFunctionSpec {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MissingFunctionSpec
    }

    fn description(&self) -> &'static str {
        "Missing function specification"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Error
    }

    fn is_enabled(&self) -> bool {
        false
    }

    fn runs_on_save_only(&self) -> bool {
        true
    }

    fn should_process_headers(&self) -> bool {
        false
    }
}

impl GenericLinter for MissingFunctionSpec {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let is_suite = ctx.sema.db.file_kind(ctx.file_id) == FileKind::TestModule;
        let testcases = if is_suite {
            match &*ctx.ct_info() {
                CommonTestInfo::Result { all, groups } => {
                    common_test::runnable_names(ctx.sema, ctx.file_id, all, groups)
                        .unwrap_or_default()
                }
                _ => FxHashSet::default(),
            }
        } else {
            FxHashSet::default()
        };
        let def_map = ctx.sema.def_map_local(ctx.file_id);
        let effective_def_map = ctx.sema.def_map(ctx.file_id);
        Some(
            def_map
                .get_functions()
                .filter(|(name, _)| {
                    effective_def_map
                        .get_function(name)
                        .is_some_and(|effective_def| {
                            let is_ct_exempt = is_suite && effective_def.exported && {
                                let is_testcase_info = name.arity() == 0
                                    && testcases.contains(&NameArity::new(name.name().clone(), 1));
                                CT_CALLBACKS.contains(*name)
                                    || testcases.contains(*name)
                                    || is_testcase_info
                            };
                            effective_def.spec.is_none()
                                && effective_def_map.get_unowned_spec(name).is_none()
                                && !is_ct_exempt
                        })
                })
                .filter_map(|(name, def)| {
                    let range = def.name_range(ctx.sema.db)?;
                    Some(GenericLinterMatchContext {
                        range: FileRange {
                            file_id: def.file.file_id,
                            range,
                        },
                        context: Context { name: name.clone() },
                    })
                })
                .collect(),
        )
    }

    fn match_description(&self, context: &Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Missing specification for function {}",
            context.name
        ))
    }
}

static CT_CALLBACKS: LazyLock<FxHashSet<NameArity>> = LazyLock::new(|| {
    [
        (*known::all, 0),
        (*known::suite, 0),
        (*known::groups, 0),
        (*known::group, 1),
        (*known::init_per_suite, 0),
        (*known::init_per_suite, 1),
        (*known::end_per_suite, 0),
        (*known::end_per_suite, 1),
        (*known::init_per_group, 1),
        (*known::init_per_group, 2),
        (*known::end_per_group, 1),
        (*known::end_per_group, 2),
        (*known::init_per_testcase, 0),
        (*known::init_per_testcase, 2),
        (*known::end_per_testcase, 0),
        (*known::end_per_testcase, 2),
    ]
    .into_iter()
    .map(|(name, arity)| NameArity::new(name, arity))
    .collect()
});

pub(crate) static LINTER: MissingFunctionSpec = MissingFunctionSpec;

#[cfg(test)]
mod tests {
    use super::LINTER;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::DiagnosticsTrigger;
    use crate::diagnostics::LintConfig;
    use crate::diagnostics::Linter;
    use crate::diagnostics::should_run;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn only_runs_on_save() {
        assert!(LINTER.runs_on_save_only());
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::MissingFunctionSpec)
            .disable(DiagnosticCode::UnreachableTest)
            .disable(DiagnosticCode::CannotEvaluateCTCallbacks)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnnecessaryFunctionArg);
        check_diagnostics_with_config(config, fixture);
    }

    #[test]
    fn missing_private_and_exported_specs() {
        check_diagnostics(
            r#"
-module(main).
-export([public/0]).
-compile(warn_missing_spec_all).
public() -> private(ok).
%%<^^^ error: W0091: Missing specification for function public/0
%%   | 💡 <suppression>
private(Value) -> Value.
%%<^^^^ error: W0091: Missing specification for function private/1
%%    | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn included_spec_is_honoured() {
        check_diagnostics(
            r#"
//- /app/src/main.erl
-module(main).
-include("contract.hrl").
-export([foo/0]).
foo() -> ok.
//- /app/src/contract.hrl
-spec foo() -> ok.
"#,
        );
    }

    #[test]
    fn suite_exports_from_headers_are_honoured() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-include("test_exports.hrl").
all() -> [testcase].
init_per_suite(Config) -> Config.
testcase() -> [].
testcase(_Config) -> ok.
//- /app/test/test_exports.hrl extra:test
-export([all/0, init_per_suite/1, testcase/0, testcase/1]).
"#,
        );
    }

    #[test]
    fn existing_specs_and_multiple_clauses() {
        check_diagnostics(
            r#"
-module(main).
-export([foo/1]).
-spec foo(atom()) -> atom().
foo(ok) -> ok;
foo(Value) -> Value.
"#,
        );
    }

    #[test]
    fn missing_spec_is_reported_once_for_multiple_clauses() {
        check_diagnostics(
            r#"
-module(main).
-export([foo/1]).
    foo(ok) -> ok;
%%  ^^^ error: W0091: Missing specification for function foo/1
%%    | 💡 <suppression>
    foo(Value) -> Value.
"#,
        );
    }

    #[test]
    fn wrong_arity_spec_does_not_cover_function() {
        check_diagnostics(
            r#"
-module(main).
-export([foo/0, foo/1]).
-spec foo() -> ok.
    foo() -> ok.
    foo(Value) -> Value.
%%  ^^^ error: W0091: Missing specification for function foo/1
%%    | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn suite_callbacks_and_known_testcases_are_exempt() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, suite/0, groups/0, group/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([a/0, a/1, b/1, helper/1, orphan/1]).
all() -> [a, {group, outer}].
suite() -> [].
groups() -> [{outer, [], [{group, inner}]}, {inner, [], [b]}].
group(_Name) -> [].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_group(_Name, Config) -> Config.
end_per_group(_Name, _Config) -> ok.
init_per_testcase(_Name, Config) -> Config.
end_per_testcase(_Name, _Config) -> ok.
a() -> [].
a(Config) -> helper(Config).
b(_Config) -> ok.
helper(Config) -> Config.
%%<^^^ error: W0091: Missing specification for function helper/1
%%   | 💡 <suppression>
orphan(_Config) -> ok.
%%<^^^ error: W0091: Missing specification for function orphan/1
%%   | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn evaluated_all_helper_still_requires_a_spec() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, testcase/1]).
all() -> do_all().
do_all() -> [testcase].
%%<^^^ error: W0091: Missing specification for function do_all/0
%%   | 💡 <suppression>
testcase(_Config) -> ok.
"#,
        );
    }

    #[test]
    fn unused_group_does_not_exempt_testcase() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, groups/0, used/1, unused/1]).
all() -> [used].
groups() -> [{unused_group, [], [unused]}].
used(_Config) -> ok.
unused(_Config) -> ok.
%%<^^^ error: W0091: Missing specification for function unused/1
%%   | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn unknown_all_does_not_exempt_exported_functions() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, candidate/1]).
all() -> erlang:error(unknown).
candidate(_Config) -> ok.
%%<^^^^^^ error: W0091: Missing specification for function candidate/1
%%      | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn callback_names_in_non_suites_require_specs() {
        check_diagnostics(
            r#"
//- /app/test/helper.erl extra:test
-module(helper).
-export([all/0, init_per_suite/1]).
    all() -> [].
%%  ^^^ error: W0091: Missing specification for function all/0
%%    | 💡 <suppression>
    init_per_suite(Config) -> Config.
%%  ^^^^^^^^^^^^^^ error: W0091: Missing specification for function init_per_suite/1
%%               | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn callback_wrong_arity_requires_a_spec() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, init_per_testcase/1]).
all() -> [].
init_per_testcase(Config) -> Config.
%%<^^^^^^^^^^^^^^ error: W0091: Missing specification for function init_per_testcase/1
%%              | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn private_callback_and_testcase_info_names_require_specs() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, testcase/1]).
all() -> [testcase].
init_per_suite(Config) -> Config.
%%<^^^^^^^^^^^ error: W0091: Missing specification for function init_per_suite/1
%%           | 💡 <suppression>
testcase() -> [].
%%<^^^^^ error: W0091: Missing specification for function testcase/0
%%     | 💡 <suppression>
testcase(_Config) -> ok.
"#,
        );
    }

    #[test]
    fn configuration_information_callbacks_are_exempt() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, init_per_suite/0, end_per_suite/0]).
-export([init_per_group/1, end_per_group/1]).
-export([init_per_testcase/0, end_per_testcase/0]).
all() -> [].
init_per_suite() -> [].
end_per_suite() -> [].
init_per_group(_Name) -> [].
end_per_group(_Name) -> [].
init_per_testcase() -> [].
end_per_testcase() -> [].
"#,
        );
    }

    #[test]
    fn comprehensions_and_inline_group_testcases_are_known() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, groups/0, a/1, b/1]).
all() -> [T || T <- [a, {group, outer}]].
groups() -> [{outer, [], [{inner, [], [b]}]}].
a(_Config) -> ok.
b(_Config) -> ok.
"#,
        );
    }

    #[test]
    fn cyclic_groups_do_not_exempt_unreachable_functions() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-export([all/0, groups/0, a/1, b/1]).
all() -> [{group, loop}].
groups() -> [{loop, [], [a, {group, loop}]}].
    a(_Config) -> ok.
    b(_Config) -> ok.
%%  ^ error: W0091: Missing specification for function b/1
%%  | 💡 <suppression>
"#,
        );
    }

    #[test]
    fn other_behaviour_callbacks_still_require_specs() {
        check_diagnostics(
            r#"
//- /app/test/main_SUITE.erl extra:test
-module(main_SUITE).
-behaviour(worker).
-export([all/0, init/1]).
all() -> [].
init(Config) -> Config.
%%<^ error: W0091: Missing specification for function init/1
%% | 💡 <suppression>
//- /app/src/worker.erl
-module(worker).
-callback init(atom()) -> atom().
"#,
        );
    }

    #[test]
    fn disabled_by_default() {
        check_diagnostics_with_config(
            DiagnosticsConfig::default(),
            r#"
-module(main).
-export([foo/0]).
foo() -> ok.
"#,
        );
    }

    #[test]
    fn normal_cli_and_project_configs_remain_disabled() {
        for lint_config in [
            LintConfig::default(),
            toml::from_str("[linters.missing_function_spec]\nseverity = 'error'")
                .expect("severity-only configuration should parse"),
            toml::from_str("[linters.compile-warn-missing-spec]\nenabled = true")
                .expect("existing missing compile warning configuration should parse"),
        ] {
            let config = DiagnosticsConfig::default()
                .configure_diagnostics(&lint_config, &[], &[])
                .expect("normal CLI configuration should succeed");
            for trigger in [
                DiagnosticsTrigger::Cli,
                DiagnosticsTrigger::Save,
                DiagnosticsTrigger::Change,
            ] {
                assert!(!should_run(
                    &LINTER, &config, &trigger, &None, false, false, false
                ));
            }
            check_diagnostics_with_config(
                config.disable(DiagnosticCode::MissingCompileWarnMissingSpec),
                r#"
-module(main).
-export([foo/0]).
foo() -> ok.
"#,
            );
        }
    }

    #[test]
    fn explicit_cli_and_project_opt_in_respect_save_only() {
        let lint_config =
            toml::from_str("[linters.missing_function_spec]\nenabled = true\nseverity = 'error'")
                .expect("explicit opt-in configuration should parse");
        let project_config = DiagnosticsConfig::default()
            .configure_diagnostics(&lint_config, &[], &[])
            .expect("project opt-in should succeed");
        let cli_config = DiagnosticsConfig::default()
            .configure_diagnostics(&LintConfig::default(), &["W0091".to_string()], &[])
            .expect("CLI opt-in should succeed");
        for config in [project_config, cli_config] {
            for trigger in [DiagnosticsTrigger::Cli, DiagnosticsTrigger::Save] {
                assert!(should_run(
                    &LINTER, &config, &trigger, &None, false, false, false
                ));
            }
            assert!(!should_run(
                &LINTER,
                &config,
                &DiagnosticsTrigger::Change,
                &None,
                false,
                false,
                false,
            ));
            check_diagnostics_with_config(
                config,
                r#"
-module(main).
-export([foo/0]).
    foo() -> ok.
%%  ^^^ error: W0091: Missing specification for function foo/0
%%    | 💡 <suppression>
"#,
            );
        }
    }

    #[test]
    fn generated_module_is_skipped() {
        check_diagnostics(&format!(
            r#"
-module(main).
%% {}generated
-export([foo/0]).
foo() -> ok.
"#,
            '@',
        ));
    }
}
