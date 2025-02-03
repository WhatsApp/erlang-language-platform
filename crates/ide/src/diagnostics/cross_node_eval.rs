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

use elp_ide_db::elp_base_db::FileId;
use hir::FunctionDef;
use hir::Semantic;
use lazy_static::lazy_static;

use super::Diagnostic;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::MakeDiagCtx;
// @fb-only: 
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        cross_node_eval(diags, sema, file_id);
    },
};

fn cross_node_eval(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.def_map(file_id)
        .get_functions()
        .for_each(|(_, def)| check_function(diags, sema, def));
}

fn check_function(diags: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    lazy_static! {
        static ref BAD_MATCHES: Vec<(FunctionMatch, Option<&'static str>)> = vec![
            vec![(FunctionMatch::m("rpc"), None)],
            vec![(FunctionMatch::mf(
                "erts_internal_dist",
                "dist_spawn_request",
            ), None)],
            vec![(FunctionMatch::mf("sys", "install"), None)],
            FunctionMatch::mfas("erlang", "spawn", vec![2, 4]).into_iter().map(|fm| (fm,None)).collect(),
            FunctionMatch::mfas("erlang", "spawn_link", vec![2, 4]).into_iter().map(|fm| (fm,None)).collect(),
            FunctionMatch::mfas("erlang", "spawn_monitor", vec![2, 4]).into_iter().map(|fm| (fm,None)).collect(),
            FunctionMatch::mfas("erlang", "spawn_opt", vec![3, 5]).into_iter().map(|fm| (fm,None)).collect(),
            FunctionMatch::mfas("sys", "install", vec![2, 3]).into_iter().map(|fm| (fm,None)).collect(),
            // @fb-only: 
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

        static ref BAD_MATCHES_MFAS: Vec<(&'static FunctionMatch, &'static Option<&'static str>)>
            = BAD_MATCHES.iter().map(|(b, m)| (b, m)).collect::<Vec<_>>();
    }

    process_badmatches(diags, sema, def, &BAD_MATCHES_MFAS);
}

fn process_badmatches(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    bad: &[(&FunctionMatch, &Option<&str>)],
) {
    find_call_in_function(
        diags,
        sema,
        def,
        bad,
        &move |ctx| match ctx.t {
            Some(m) => Some(*m),
            None => Some(r#"Production code must not use cross node eval (e.g. `rpc:call()`)"#),
        },
        &move |ctx @ MakeDiagCtx {
                   sema,
                   def_fb,
                   extra,
                   ..
               }: MakeDiagCtx<'_, &str>| {
            let diag = Diagnostic::new(DiagnosticCode::CrossNodeEval, *extra, ctx.range_mf_only())
                .with_severity(Severity::Error)
                .with_ignore_fix(sema, def_fb.file_id());
            Some(diag)
        },
    );
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use expect_test::Expect;

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
            %%  ^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)
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
            %%  ^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)
                erts_internal_dist:dist_spawn_request(Node, modu, ff, [], []).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)
                rpc:multicall([Node], mod, func, []).
            %%  ^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)
                sys:install(Name, FuncSpec, 500).
            %%  ^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
            %%  ^^^^^^^^^^^^^ 💡 error: Production code must not use cross node eval (e.g. `rpc:call()`)

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
