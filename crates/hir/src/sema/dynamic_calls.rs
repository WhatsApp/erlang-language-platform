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

use elp_base_db::DynamicCallPatternInput;
use elp_base_db::PatternKey;
use elp_base_db::build_index;
use elp_syntax::SmolStr;
use fxhash::FxHashMap;

/// Returns the set of built-in OTP dynamic call patterns as `DynamicCallPatternInput`.
/// These are the standard Erlang/OTP functions that perform dynamic calls
/// (apply, spawn, rpc:call, etc.), expressed in the same Erlang-like syntax
/// as user-configured patterns.
pub(crate) fn builtin_dynamic_call_patterns() -> Vec<DynamicCallPatternInput> {
    let patterns = [
        // === Dynamic call patterns (Function + Args/Arity) ===
        // erlang module (implicit)
        "apply(Function, Args)",
        "apply(Module, Function, Args)",
        "function_exported(Module, Function, Arity)",
        "hibernate(Module, Function, Args)",
        "is_builtin(Module, Function, Arity)",
        "spawn_link(Module, Function, Args)",
        "spawn_link(_, Module, Function, Args)",
        "spawn_monitor(Module, Function, Args)",
        "spawn_monitor(_, Module, Function, Args)",
        "spawn_opt(Module, Function, Args, _)",
        "spawn_opt(_, Module, Function, Args, _)",
        "spawn_request(_, Module, Function, Args, _)",
        "spawn(Module, Function, Args)",
        "spawn(_, Module, Function, Args)",
        // erlang module (explicit)
        "erlang:apply(Function, Args)",
        "erlang:apply(Module, Function, Args)",
        "erlang:function_exported(Module, Function, Arity)",
        "erlang:hibernate(Module, Function, Args)",
        "erlang:is_builtin(Module, Function, Arity)",
        "erlang:spawn_link(Module, Function, Args)",
        "erlang:spawn_link(_, Module, Function, Args)",
        "erlang:spawn_monitor(Module, Function, Args)",
        "erlang:spawn_monitor(_, Module, Function, Args)",
        "erlang:spawn_opt(Module, Function, Args, _)",
        "erlang:spawn_opt(_, Module, Function, Args, _)",
        "erlang:spawn_request(_, Module, Function, Args, _)",
        "erlang:spawn(Module, Function, Args)",
        "erlang:spawn(_, Module, Function, Args)",
        // rpc
        "rpc:call(_, Module, Function, Args)",
        "rpc:call(_, Module, Function, Args, _)",
        "rpc:async_call(_, Module, Function, Args)",
        "rpc:cast(_, Module, Function, Args)",
        "rpc:multicall(Module, Function, Args)",
        "rpc:multicall(Module, Function, Args, _)",
        "rpc:multicall(_, Module, Function, Args, _)",
        // erpc
        "erpc:call(_, Module, Function, Args)",
        "erpc:call(_, Module, Function, Args, _)",
        "erpc:cast(_, Module, Function, Args)",
        "erpc:multicall(_, Module, Function, Args)",
        "erpc:multicall(_, Module, Function, Args, _)",
        "erpc:multicast(_, Module, Function, Args)",
        "erpc:send_request(_, Module, Function, Args, _, _)",
        // peer
        "peer:call(_, Module, Function, Args)",
        "peer:call(_, Module, Function, Args, _)",
        "peer:cast(_, Module, Function, Args)",
        // meck - atom patterns
        "meck:called(Module, _, _)",
        "meck:called(Module, _, _, _)",
        "meck:capture(_, Module, _, _, _)",
        "meck:capture(_, Module, _, _, _, _)",
        "meck:history(Module)",
        "meck:history(Module, _)",
        "meck:num_calls(Module, _, _)",
        "meck:num_calls(Module, _, _, _)",
        "meck:wait(Module, _, _, _)",
        "meck:wait(_, Module, _, _, _)",
        "meck:wait(_, Module, _, _, _, _)",
        // meck - functions accepting both atom and list forms
        "meck:delete(Module, _, _)",
        "meck:delete([Module], _, _)",
        "meck:delete(Module, _, _, _)",
        "meck:delete([Module], _, _, _)",
        "meck:expect(Module, _, _)",
        "meck:expect([Module], _, _)",
        "meck:expect(Module, _, _, _)",
        "meck:expect([Module], _, _, _)",
        "meck:expects(Module, _)",
        "meck:expects([Module], _)",
        "meck:loop(Module, _, _, _)",
        "meck:loop([Module], _, _, _)",
        "meck:new(Module)",
        "meck:new([Module])",
        "meck:new(Module, _)",
        "meck:new([Module], _)",
        "meck:reset(Module)",
        "meck:reset([Module])",
        "meck:sequence(Module, _, _, _)",
        "meck:sequence([Module], _, _, _)",
        "meck:unload(Module)",
        "meck:unload([Module])",
        "meck:validate(Module)",
        "meck:validate([Module])",
        // code module - atom patterns
        "code:load_file(Module)",
        "code:ensure_loaded(Module)",
        "code:delete(Module)",
        "code:purge(Module)",
        "code:soft_purge(Module)",
        "code:is_loaded(Module)",
        "code:get_object_code(Module)",
        "code:module_md5(Module)",
        "code:is_sticky(Module)",
    ];
    patterns
        .iter()
        .map(|s| {
            elp_base_db::parse_dynamic_call_pattern(s)
                .unwrap_or_else(|e| panic!("invalid built-in pattern '{}': {}", s, e))
        })
        .collect()
}

static BUILTIN: LazyLock<FxHashMap<PatternKey, DynamicCallPatternInput>> = LazyLock::new(|| {
    let mut patterns = builtin_dynamic_call_patterns();
    // @fb-only: patterns.extend(crate::sema::meta_only::meta_only_dynamic_call_patterns());
    build_index(patterns)
});

/// Look up a dynamic call pattern by call signature.
///
/// Returns the merged pattern for `(module, function, arity)`. The
/// returned pattern's `module_arg_shape` indicates whether the call
/// site can use `Module` (atom), `[Module]` (list), or `Either` —
/// callers branch on the shape to handle the actual call-site arg.
///
/// Built-in patterns take precedence over user-configured extras for
/// the same signature; both indices are O(1) `FxHashMap` lookups.
///
/// Returns `Cow::Borrowed` for built-in patterns (zero-cost, no heap
/// allocation) and `Cow::Owned` for extra patterns (cloned from the
/// salsa-managed `Arc`).
pub fn lookup_pattern<'a>(
    db: &dyn elp_base_db::SourceDatabase,
    module: Option<&str>,
    function: &str,
    arity: usize,
) -> Option<Cow<'a, DynamicCallPatternInput>> {
    let key = (
        module.map(SmolStr::new),
        SmolStr::new(function),
        arity as u32,
    );

    BUILTIN.get(&key).map(Cow::Borrowed).or_else(|| {
        db.extra_dynamic_call_patterns()
            .get(&key)
            .cloned()
            .map(Cow::Owned)
    })
}
