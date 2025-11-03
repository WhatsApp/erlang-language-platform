/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: cross_node_eval
//!
//! Return a diagnostic for rpc calls to remote nodes.

use crate::codemod_helpers::FunctionMatch;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::lazy_function_matches;

pub(crate) struct CrossNodeEvalLinter;

impl Linter for CrossNodeEvalLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::CrossNodeEval
    }
    fn description(&self) -> &'static str {
        "Production code must not use cross node eval (e.g. `rpc:call()`)"
    }
    fn severity(&self) -> Severity {
        Severity::Error
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl FunctionCallLinter for CrossNodeEvalLinter {
    type Context = ();

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![
            vec![
                vec![FunctionMatch::m("rpc")],
                vec![FunctionMatch::mf(
                    "erts_internal_dist",
                    "dist_spawn_request"
                )],
                vec![FunctionMatch::mf("sys", "install")],
                FunctionMatch::mfas("erlang", "spawn", vec![2, 4]),
                FunctionMatch::mfas("erlang", "spawn_link", vec![2, 4]),
                FunctionMatch::mfas("erlang", "spawn_monitor", vec![2, 4]),
                FunctionMatch::mfas("erlang", "spawn_opt", vec![3, 5]),
                FunctionMatch::mfas("sys", "install", vec![2, 3]),
            ]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
        ]
    }
}

pub static LINTER: CrossNodeEvalLinter = CrossNodeEvalLinter;

#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(config, fixture)
    }

    #[track_caller]
    pub(crate) fn check_fix(fixture_before: &str, fixture_after: Expect) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
        check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[test]
    fn local_ok() {
        check_diagnostics(
            r#"
            -module(main).

            foo() ->
                erlang:spawn(fun() -> ok end),
                spawn(fun() -> ok end),
                erlang:spawn_link(fun() -> ok end),
                spawn_link(fun() -> ok end),
                erlang:spawn_monitor(fun() -> ok end),
                spawn_monitor(fun() -> ok end),
                erlang:spawn_opt(fun() -> ok end),
                spawn_opt(fun() -> ok end).
            "#,
        )
    }
    #[test]
    fn spawn_2() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn(Node, fun() -> ok end).
            %%  ^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)
            "#,
        )
    }

    #[test]
    fn spawn_2_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn(Node, fun() -> ok end).
            %%  ^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_4() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn(Node, modu, ff, []).
            %%  ^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_4_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn(Node, modu, ff, []).
            %%  ^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    // -----------------------------------------------------------------

    #[test]
    fn spawn_link_2() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn_link(Node, fun() -> ok end).
            %%  ^^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)
            "#,
        )
    }

    #[test]
    fn spawn_link_2_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn_link(Node, fun() -> ok end).
            %%  ^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_link_4() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn_link(Node, modu, ff, []).
            %%  ^^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_link_4_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn_link(Node, modu, ff, []).
            %%  ^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    // -----------------------------------------------------------------

    #[test]
    fn spawn_monitor_2() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn_monitor(Node, fun() -> ok end).
            %%  ^^^^^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)
            "#,
        )
    }

    #[test]
    fn spawn_monitor_2_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn_monitor(Node, fun() -> ok end).
            %%  ^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_monitor_4() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn_monitor(Node, modu, ff, []).
            %%  ^^^^^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_monitor_4_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn_monitor(Node, modu, ff, []).
            %%  ^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    // -----------------------------------------------------------------

    #[test]
    fn spawn_opt_3() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn_opt(Node, fun() -> ok end, []).
            %%  ^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)
            "#,
        )
    }

    #[test]
    fn spawn_opt_3_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn_opt(Node, fun() -> ok end, []).
            %%  ^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_opt_5() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn_opt(Node, modu, ff, [], []).
            %%  ^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn spawn_opt_5_bare() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                spawn_opt(Node, modu, ff, [], []).
            %%  ^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn erts_internal_dist_dist_spawn_request() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                erts_internal_dist:dist_spawn_request(Node, fun() -> ok end),
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)
                erts_internal_dist:dist_spawn_request(Node, modu, ff, [], []).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn rpc() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Node) ->
                rpc:call(Node, mod, func, []),
            %%  ^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)
                rpc:multicall([Node], mod, func, []).
            %%  ^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn sys_install() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Name, FuncSpec) ->
                sys:install(Name, FuncSpec),
            %%  ^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)
                sys:install(Name, FuncSpec, 500).
            %%  ^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
        )
    }

    #[test]
    fn sys_install_ignored() {
        check_diagnostics(
            r#"
            -module(main).

            foo(Name, FuncSpec) ->
                % elp:ignore W0014
                sys:install(Name, FuncSpec),
                % elp:ignore cross_node_eval
                sys:install(Name, FuncSpec, 500).

            "#,
        )
    }

    #[test]
    fn elp_ignore_fix() {
        check_fix(
            r#"
            -module(main).

            foo(Name, FuncSpec) ->
                sys:inst~all(Name, FuncSpec).
            %%  ^^^^^^^^^^^^^ 💡 error: W0014: Production code must not use cross node eval (e.g. `rpc:call()`)

            "#,
            expect![[r#"
            -module(main).

            foo(Name, FuncSpec) ->
                % elp:ignore W0014 (cross_node_eval)
                sys:install(Name, FuncSpec).

            "#]],
        )
    }
}
