/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: unused_callback
//!
//! Return a diagnostic for a `-callback` that nothing calls: its name appears
//! in none of the behaviour module's functions, and no module in the project
//! calls it dynamically, as `Mod:Callback(...)`, `fun Mod:Callback/N`, or
//! passes its name to a function such as `apply/3`, also inside a tuple, list,
//! map or record argument.

use std::borrow::Cow;

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use fxhash::FxHashSet;
use hir::AnyExpr;
use hir::Atom;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FoldBody;
use hir::InFile;
use hir::Literal;
use hir::Name;
use hir::NameArity;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::sema::dynamic_calls::lookup_dynamic_pattern;

use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct UnusedCallbackLinter;

impl Linter for UnusedCallbackLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedCallback
    }

    fn description(&self) -> &'static str {
        "Callback is never called."
    }

    // Searches the whole project for dynamic calls.
    fn runs_on_save_only(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Context {
    name: NameArity,
}

impl GenericLinter for UnusedCallbackLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let def_map_local = sema.def_map_local(ctx.file_id);
        let callbacks = def_map_local.get_callbacks();
        if callbacks.is_empty() {
            return None;
        }
        let atoms = atoms_in_functions(ctx.sema, ctx.file_id);
        let unused: Vec<&NameArity> = callbacks
            .keys()
            .filter(|name| !atoms.contains(&Atom::new(name.name())))
            .collect();
        let unused = not_dispatched_elsewhere(ctx, unused);
        let mut res: Vec<_> = callbacks
            .iter()
            .filter(|(name, _)| unused.contains(name))
            .filter_map(|(name, def)| {
                let fun = def.source(sema.db.upcast()).fun()?;
                Some(GenericLinterMatchContext {
                    range: FileRange {
                        file_id: def.file.file_id,
                        range: fun.syntax().text_range(),
                    },
                    context: Context { name: name.clone() },
                })
            })
            .collect();
        res.sort_by_key(|m| m.range.range.start());
        Some(res)
    }

    fn match_description(&self, context: &Context) -> Cow<'_, str> {
        Cow::Owned(format!("Callback `{}` is never called.", context.name))
    }
}

pub(crate) static LINTER: UnusedCallbackLinter = UnusedCallbackLinter;

const STRATEGY: Strategy = Strategy {
    macros: MacroStrategy::Expand,
    parens: ParenStrategy::InvisibleParens,
};

/// Every atom in the bodies of the module's functions, including functions
/// from included headers.
fn atoms_in_functions(sema: &Semantic, file_id: FileId) -> FxHashSet<Atom> {
    let mut atoms = FxHashSet::default();
    sema.def_map(file_id).get_functions().for_each(|(_, def)| {
        def.in_function_body(sema, def).fold_function(
            STRATEGY,
            (),
            &mut |_acc, _clause_id, fold_ctx| match fold_ctx.item {
                AnyExpr::Expr(Expr::Literal(Literal::Atom(atom)))
                | AnyExpr::Pat(Pat::Literal(Literal::Atom(atom))) => {
                    atoms.insert(atom);
                }
                _ => {}
            },
        );
    });
    atoms
}

/// The callbacks of `candidates` that no other module of the project calls
/// dynamically.
fn not_dispatched_elsewhere<'a>(
    ctx: &LinterContext,
    mut candidates: Vec<&'a NameArity>,
) -> Vec<&'a NameArity> {
    let sema = ctx.sema;
    let Some(app) = sema.db.file_app(ctx.file_id) else {
        return candidates;
    };
    let module_index = sema.db.module_index(app.project(sema.db));
    let mut unmentioned = FxHashSet::default();
    for (_, file_id) in module_index.iter_own() {
        if candidates.is_empty() {
            break;
        }
        if file_id == ctx.file_id {
            continue;
        }
        if !mentions_candidate(sema, file_id, &candidates, &mut unmentioned) {
            continue;
        }
        let dispatched = dynamic_dispatch(sema, file_id);
        candidates.retain(|name| {
            let atom = Atom::new(name.name());
            !dispatched.contains(&(atom, Some(name.arity()))) && !dispatched.contains(&(atom, None))
        });
    }
    candidates
}

/// Whether the text of `file_id`, or of a header it includes directly or
/// transitively, mentions one of `candidates`, since a dynamic call can come
/// from a macro defined in a header. Only parses the files, so modules that
/// cannot call a candidate are never preprocessed. `unmentioned` holds the
/// files whose own text mentions no candidate, which stays true as candidates
/// are removed, so a header shared by many modules is searched once.
fn mentions_candidate(
    sema: &Semantic,
    file_id: FileId,
    candidates: &[&NameArity],
    unmentioned: &mut FxHashSet<FileId>,
) -> bool {
    let db = sema.db;
    let orig_app = db.file_app(file_id);
    let mut visited = FxHashSet::default();
    let mut pending = vec![file_id];
    while let Some(id) = pending.pop() {
        if !visited.insert(id) {
            continue;
        }
        if !unmentioned.contains(&id) {
            let text = db.file(id).text(db);
            if candidates
                .iter()
                .any(|name| text.contains(name.name().as_str()))
            {
                return true;
            }
            unmentioned.insert(id);
        }
        pending.extend(
            sema.form_list(id)
                .includes()
                .filter_map(|(idx, _)| db.resolve_include(orig_app, InFile::new(id, idx))),
        );
    }
    false
}

/// Function names a module may call on a module only known at runtime: the
/// name and arity of remote calls and captures whose module is not an atom
/// literal, and atoms passed as arguments, as in `apply(Mod, Name, Args)`,
/// also inside tuples, lists, maps and records, as in
/// `dispatch(Mod, {Name, Arg})`.
/// When the module also calls `Mod:Fun(...)`, `fun Mod:Fun/N`, or a function
/// matching a dynamic call pattern, such as `apply/3` or `spawn/3`, with both
/// the module and the function unknown, any atom in it may name the function.
fn dynamic_dispatch(sema: &Semantic, file_id: FileId) -> FxHashSet<(Atom, Option<u32>)> {
    let mut res = FxHashSet::default();
    let mut all_atoms = FxHashSet::default();
    let mut fully_dynamic = false;
    sema.def_map(file_id).get_functions().for_each(|(_, def)| {
        let def_fb = def.in_function_body(sema, def);
        def_fb.fold_function(STRATEGY, (), &mut |_acc, clause_id, fold_ctx| {
            let clause_body = def_fb.body(clause_id);
            let body = clause_body.index_with_strategy(STRATEGY);
            let atom = |expr_id: ExprId| match &body[expr_id] {
                Expr::Literal(Literal::Atom(atom)) => Some(*atom),
                _ => None,
            };
            let unknown = |expr_id: ExprId| atom(expr_id).is_none();
            // A call matching a dynamic call pattern, such as
            // `apply(Mod, Fun, Args)`, with both the module and the function
            // unknown.
            let calls_unknown_function = |target: &CallTarget<ExprId>, args: &[ExprId]| {
                let (module, function) = match target {
                    CallTarget::Local { name } => (None, *name),
                    CallTarget::Remote { module, name, .. } => match atom(*module) {
                        Some(module) => (Some(module.as_name()), *name),
                        None => return false,
                    },
                };
                let Some(function) = atom(function) else {
                    return false;
                };
                let Some(pattern) = lookup_dynamic_pattern(
                    sema.db.upcast(),
                    module.as_ref().map(Name::as_str),
                    function.as_name().as_str(),
                    args.len(),
                ) else {
                    return false;
                };
                let arg = |index: Option<u32>| args.get(index? as usize);
                matches!(
                    (arg(pattern.module_arg_index), arg(pattern.function_arg_index)),
                    (Some(module), Some(function)) if unknown(*module) && unknown(*function)
                )
            };
            let mut add_remote = |target: &CallTarget<ExprId>, arity: Option<u32>| {
                if let CallTarget::Remote { module, name, .. } = target
                    && atom(*module).is_none()
                    && let Some(name) = atom(*name)
                {
                    res.insert((name, arity));
                }
            };
            match &fold_ctx.item {
                AnyExpr::Expr(Expr::Literal(Literal::Atom(atom)))
                | AnyExpr::Pat(Pat::Literal(Literal::Atom(atom))) => {
                    all_atoms.insert(*atom);
                }
                AnyExpr::Expr(Expr::Call { target, args }) => {
                    let remote_unknown = matches!(
                        target,
                        CallTarget::Remote { module, name, .. } if unknown(*module) && unknown(*name)
                    );
                    if remote_unknown || calls_unknown_function(target, args) {
                        fully_dynamic = true;
                    }
                }
                AnyExpr::Expr(Expr::CaptureFun {
                    target: CallTarget::Remote { module, name, .. },
                    ..
                }) if unknown(*module) && unknown(*name) => {
                    fully_dynamic = true;
                }
                _ => {}
            }
            match &fold_ctx.item {
                AnyExpr::Expr(Expr::Call { target, args }) => {
                    add_remote(target, Some(args.len() as u32));
                    args.iter()
                        .for_each(|arg| insert_nested_atoms(&body, *arg, &mut res));
                }
                AnyExpr::Expr(Expr::CaptureFun { target, arity }) => {
                    let arity = match &body[*arity] {
                        Expr::Literal(Literal::Integer(arity)) => u32::try_from(arity.value).ok(),
                        _ => None,
                    };
                    add_remote(target, arity);
                }
                _ => {}
            }
        });
    });
    if fully_dynamic {
        res.extend(all_atoms.into_iter().map(|atom| (atom, None)));
    }
    res
}

fn insert_nested_atoms(body: &FoldBody, expr_id: ExprId, res: &mut FxHashSet<(Atom, Option<u32>)>) {
    match &body[expr_id] {
        Expr::Literal(Literal::Atom(atom)) => {
            res.insert((*atom, None));
        }
        Expr::Tuple { exprs } => exprs
            .iter()
            .for_each(|expr| insert_nested_atoms(body, *expr, res)),
        Expr::List { exprs, tail } => exprs
            .iter()
            .chain(tail)
            .for_each(|expr| insert_nested_atoms(body, *expr, res)),
        Expr::Map { fields } => fields
            .iter()
            .flat_map(|(key, value)| [key, value])
            .for_each(|expr| insert_nested_atoms(body, *expr, res)),
        Expr::MapUpdate { fields, .. } => fields
            .iter()
            .flat_map(|(key, _, value)| [key, value])
            .for_each(|expr| insert_nested_atoms(body, *expr, res)),
        Expr::Record {
            fields,
            default_field,
            ..
        } => fields
            .iter()
            .map(|(_, value)| value)
            .chain(default_field)
            .for_each(|expr| insert_nested_atoms(body, *expr, res)),
        Expr::RecordUpdate { fields, .. }
        | Expr::NativeRecord { fields, .. }
        | Expr::NativeRecordUpdate { fields, .. } => fields
            .iter()
            .for_each(|(_, value)| insert_nested_atoms(body, *value, res)),
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use super::LINTER;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::Linter;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn only_runs_on_save() {
        assert!(LINTER.runs_on_save_only());
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::CrossNodeEval);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn unused_callback() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -export([run/2]).
            -callback init(term()) -> ok.
            -callback handle(term()) -> ok.
            %%        ^^^^^^ warning: W0098: Callback `handle/1` is never called.
            %%             | 💡 <suppression>
            -callback terminate() -> ok.
            %%        ^^^^^^^^^ warning: W0098: Callback `terminate/0` is never called.
            %%                | 💡 <suppression>
            -optional_callbacks([terminate/0]).
            run(Mod, Arg) -> Mod:init(Arg).
            "#,
        )
    }

    #[test]
    fn used_callbacks() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -export([run/2]).
            -callback a(term()) -> ok.
            -callback b(term()) -> ok.
            -callback c() -> ok.
            -callback d() -> ok.
            -callback e() -> ok.
            -define(CALL(M, F), M:F()).
            run(Mod, Arg) ->
                F = fun Mod:a/1,
                F(Arg),
                apply(Mod, b, [Arg]),
                case erlang:function_exported(Mod, c, 0) of
                    true -> ?CALL(Mod, d);
                    false -> ok
                end,
                dispatch(Mod).
            dispatch(Mod) ->
                case Mod of
                    e -> ok;
                    _ -> ok
                end.
            "#,
        )
    }

    #[test]
    fn callback_dispatched_from_other_module() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback a(term()) -> ok.
            -callback b(term()) -> ok.
            -callback c(term()) -> ok.
            -callback d(term()) -> ok.
            %%        ^ warning: W0098: Callback `d/1` is never called.
            %%        | 💡 <suppression>
            -callback e(term()) -> ok.
            %%        ^ warning: W0098: Callback `e/1` is never called.
            %%        | 💡 <suppression>
            //- /src/dispatcher.erl
            -module(dispatcher).
            -export([run/2]).
            run(Mod, Arg) ->
                Mod:a(Arg),
                F = fun Mod:b/1,
                F(Arg),
                wrapper:call(Mod, c, [Arg]),
                Mod:d(Arg, Arg),
                other:e(Arg).
            "#,
        )
    }

    #[test]
    fn callback_passed_inside_tuple_list_or_map() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback a(term()) -> ok.
            -callback b(term()) -> ok.
            -callback c(term()) -> ok.
            -callback d(term()) -> ok.
            %%        ^ warning: W0098: Callback `d/1` is never called.
            %%        | 💡 <suppression>
            //- /src/caller.erl
            -module(caller).
            -export([run/2]).
            run(Mod, Arg) ->
                dispatcher:call(Mod, {a, Arg}),
                dispatcher:call(Mod, [b, Arg]),
                dispatcher:call(Mod, #{name => c}),
                dispatcher:call(Mod, fun() -> d end).
            "#,
        )
    }

    #[test]
    fn callback_passed_inside_map_update_or_record() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback a(term()) -> ok.
            -callback b(term()) -> ok.
            -callback c(term()) -> ok.
            -callback d(term()) -> ok.
            %%        ^ warning: W0098: Callback `d/1` is never called.
            %%        | 💡 <suppression>
            //- /src/caller.erl
            -module(caller).
            -export([run/3]).
            -record(req, {name, arg}).
            run(Mod, Req, Opts) ->
                dispatcher:call(Mod, Opts#{name => a}),
                dispatcher:call(Mod, #req{name = b}),
                dispatcher:call(Mod, Req#req{name = c}),
                dispatcher:call(Mod, Req#req.arg).
            "#,
        )
    }

    #[test]
    fn callback_passed_through_macro_or_parentheses() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback a(term()) -> ok.
            -callback b(term()) -> ok.
            -callback c(term()) -> ok.
            -callback d(term()) -> ok.
            %%        ^ warning: W0098: Callback `d/1` is never called.
            %%        | 💡 <suppression>
            //- /src/caller.erl
            -module(caller).
            -export([run/2]).
            -define(CB, a).
            -define(REQ(Name), {Name, req}).
            run(Mod, Arg) ->
                dispatcher:call(Mod, ?CB, Arg),
                dispatcher:call(Mod, ?REQ(b)),
                dispatcher:call(Mod, (c), Arg),
                dispatcher:call(Mod, fun() -> d end).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_through_spawn_or_rpc() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback on_a() -> ok.
            -callback on_b() -> ok.
            -callback on_c() -> ok.
            -callback on_d() -> ok.
            %%        ^^^^ warning: W0098: Callback `on_d/0` is never called.
            %%           | 💡 <suppression>
            //- /src/spawner.erl
            -module(spawner).
            -export([start/2]).
            start(Mod, Kind) ->
                F = case Kind of a -> on_a; _ -> other end,
                spawn(Mod, F, []).
            //- /src/linker.erl
            -module(linker).
            -export([start/3]).
            start(Node, Mod, F) ->
                proc_lib:spawn_link(Node, Mod, F, []),
                on_b.
            //- /src/caller.erl
            -module(caller).
            -export([run/3]).
            run(Node, Mod, F) ->
                rpc:call(Node, Mod, F, []),
                on_c.
            //- /src/known_module.erl
            -module(known_module).
            -export([start/2]).
            start(Node, F) ->
                spawn(Node, other, F, []),
                on_d.
            //- /src/other.erl
            -module(other).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_through_proc_lib_start_or_multicall() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback on_a() -> ok.
            -callback on_b() -> ok.
            -callback on_c() -> ok.
            -callback on_d() -> ok.
            %%        ^^^^ warning: W0098: Callback `on_d/0` is never called.
            %%           | 💡 <suppression>
            //- /src/starter.erl
            -module(starter).
            -export([start/2]).
            start(Mod, Kind) ->
                F = case Kind of a -> on_a; _ -> other end,
                proc_lib:start_link(Mod, F, []).
            //- /src/nodes_first.erl
            -module(nodes_first).
            -export([run/3]).
            run(Nodes, Mod, F) ->
                rpc:multicall(Nodes, Mod, F, []),
                on_b.
            //- /src/timeout_last.erl
            -module(timeout_last).
            -export([run/2]).
            run(Mod, F) ->
                rpc:multicall(Mod, F, [], 5000),
                on_c.
            //- /src/known_module.erl
            -module(known_module).
            -export([run/2]).
            run(Nodes, F) ->
                rpc:multicall(Nodes, other, F, []),
                on_d.
            //- /src/other.erl
            -module(other).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_through_configured_dynamic_call() {
        check_diagnostics(
            r#"
            //- dynamic_call: my_rpc:call(_, Module, Function, Args)
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback on_a() -> ok.
            -callback on_b() -> ok.
            %%        ^^^^ warning: W0098: Callback `on_b/0` is never called.
            %%           | 💡 <suppression>
            //- /src/caller.erl
            -module(caller).
            -export([run/3]).
            run(Node, Mod, F) ->
                my_rpc:call(Node, Mod, F, []),
                on_a.
            //- /src/known_module.erl
            -module(known_module).
            -export([run/2]).
            run(Node, F) ->
                my_rpc:call(Node, other, F, []),
                on_b.
            //- /src/my_rpc.erl
            -module(my_rpc).
            -export([call/4]).
            call(_, _, _, _) -> ok.
            //- /src/other.erl
            -module(other).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_through_generic_dispatcher_module() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback on_a(term()) -> ok.
            -callback on_b(term()) -> ok.
            -callback on_c(term()) -> ok.
            %%        ^^^^ warning: W0098: Callback `on_c/1` is never called.
            %%           | 💡 <suppression>
            //- /src/dispatcher.erl
            -module(dispatcher).
            -export([call/3]).
            call(Mod, F, Arg) -> Mod:F(Arg).
            //- /src/caller.erl
            -module(caller).
            -export([run/2]).
            run(Mod, Arg) ->
                dispatcher:call(Mod, on_a, Arg),
                dispatcher:call(Mod, {on_b}, Arg),
                F = on_c,
                dispatcher:call(Mod, F, Arg).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_from_header_macro() {
        check_diagnostics(
            r#"
            //- /include/metrics.hrl include_path:/include
            -define(COUNT(Metric), (registry:get(metrics_module)):count(Metric)).
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback count(term()) -> ok.
            //- /src/user.erl
            -module(user).
            -export([run/0]).
            -include("metrics.hrl").
            run() -> ?COUNT(requests).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_from_nested_header_macro() {
        check_diagnostics(
            r#"
            //- /include/metrics.hrl include_path:/include
            -define(COUNT(Metric), (registry:get(metrics_module)):count(Metric)).
            //- /include/common.hrl include_path:/include
            -include("metrics.hrl").
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback count(term()) -> ok.
            //- /src/user.erl
            -module(user).
            -export([run/0]).
            -include("common.hrl").
            run() -> ?COUNT(requests).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_through_variable_name() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback on_a(term()) -> ok.
            -callback on_b(term()) -> ok.
            //- /src/dispatcher.erl
            -module(dispatcher).
            -export([run/3]).
            run(Mod, Phase, Arg) ->
                F = case Phase of
                    a -> on_a;
                    b -> on_b
                end,
                Mod:F(Arg).
            "#,
        )
    }

    #[test]
    fn callback_dispatched_through_variable_capture_or_apply() {
        check_diagnostics(
            r#"
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback on_a(term()) -> ok.
            -callback on_b(term()) -> ok.
            -callback on_c(term()) -> ok.
            -callback on_d(term()) -> ok.
            %%        ^^^^ warning: W0098: Callback `on_d/1` is never called.
            %%           | 💡 <suppression>
            //- /src/capture.erl
            -module(capture).
            -export([run/3]).
            run(Mod, F, Arg) ->
                G = fun Mod:F/1,
                G(Arg),
                on_a.
            //- /src/applier.erl
            -module(applier).
            -export([run/3]).
            run(Mod, F, Arg) ->
                apply(Mod, F, [Arg]),
                on_b.
            //- /src/qualified_applier.erl
            -module(qualified_applier).
            -export([run/3]).
            run(Mod, F, Arg) ->
                erlang:apply(Mod, F, [Arg]),
                on_c.
            //- /src/known_module.erl
            -module(known_module).
            -export([run/2]).
            run(F, Arg) ->
                G = fun other:F/1,
                G(Arg),
                apply(other, F, [Arg]),
                on_d.
            //- /src/other.erl
            -module(other).
            "#,
        )
    }

    #[test]
    fn callback_used_from_header_function() {
        check_diagnostics(
            r#"
            //- /include/dispatch.hrl include_path:/include
            dispatch(Mod) -> Mod:handle().
            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -export([run/1]).
            -include("dispatch.hrl").
            -callback handle() -> ok.
            run(Mod) -> dispatch(Mod).
            "#,
        )
    }
}
