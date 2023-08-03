/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: cross_node_eval
//!
//! Return a diagnostic for rpc calls to remote nodes.
//!

use elp_ide_db::elp_base_db::FileId;
use hir::FunctionDef;
use hir::Semantic;
use lazy_static::lazy_static;

use super::Diagnostic;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::FunctionMatch;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;

pub(crate) fn cross_node_eval(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    if sema.db.is_generated(file_id) {
        return;
    }
    sema.def_map(file_id)
        .get_functions()
        .iter()
        .for_each(|(_arity, def)| check_function(diags, sema, def));
}

pub(crate) fn check_function(diags: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    lazy_static! {
        static ref BAD_MATCHES: Vec<FunctionMatch> = vec![
            vec![FunctionMatch::m("rpc")],
            vec![FunctionMatch::mf(
                "erts_internal_dist",
                "dist_spawn_request",
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
        .collect::<Vec<_>>();
    }

    process_badmatches(diags, sema, def, &BAD_MATCHES);
}

pub(crate) fn process_badmatches(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    bad: &[FunctionMatch],
) {
    let mfas = bad.iter().map(|b| (b, ())).collect::<Vec<_>>();
    find_call_in_function(
        diags,
        sema,
        def,
        &mfas,
        &move |_mfa, _, _target, _args, _def_fb| {
            Some(r#"Production code must not use cross node eval (e.g. `rpc:call()`)"#.to_string())
        },
        move |_sema, mut _def_fb, _target, _args, extra_info, range| {
            let diag = Diagnostic::new(DiagnosticCode::CrossNodeEval, extra_info, range.clone())
                .severity(Severity::Warning);
            Some(diag)
        },
    );
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[track_caller]
    pub(crate) fn check_diagnostics(ra_fixture: &str) {
        let mut config = DiagnosticsConfig::default();
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);
        check_diagnostics_with_config(config, ra_fixture)
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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)
                erts_internal_dist:dist_spawn_request(Node, modu, ff, [], []).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)
                rpc:multicall([Node], mod, func, []).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)
                sys:install(Name, FuncSpec, 500).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Production code must not use cross node eval (e.g. `rpc:call()`)

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
}
