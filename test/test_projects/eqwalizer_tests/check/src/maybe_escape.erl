%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(maybe_escape).
-feature(maybe_expr, enable).

-compile([export_all, nowarn_export_all]).

%% Without an `else`, a `?=` that does not match makes the whole block evaluate
%% to the value that failed to match, so `true ?= <test>` lets `false` escape
%% and the block is not just `integer()`.

-spec guard_escape_check(term()) -> integer().
guard_escape_check(X) ->
    maybe
        true ?= is_atom(X),
        1
    end.

-spec guard_escape_eval(term()) -> integer().
guard_escape_eval(X) ->
    R = maybe
            true ?= is_atom(X),
            1
        end,
    R.

%% The `{ok, _} ?= ...` idiom: only the values that fail to match escape, so the
%% block is `ok | {error, atom()}` and never `{ok, integer()}`.

-spec res() -> {ok, integer()} | {error, atom()}.
res() ->
    {ok, 1}.

-spec result_escape_check() -> ok | {error, atom()}.
result_escape_check() ->
    maybe
        {ok, _} ?= res(),
        ok
    end.

-spec result_escape_eval() -> ok | {error, atom()}.
result_escape_eval() ->
    R = maybe
            {ok, _} ?= res(),
            ok
        end,
    R.

%% `true ?= <expr>` where the right-hand side is not a test: `true` is the value
%% that makes the block continue, so it is the one value that cannot escape.

-spec flag() -> boolean().
flag() ->
    true.

-spec orelse_escape_check(integer()) -> integer().
orelse_escape_check(N) ->
    maybe
        true ?= flag() orelse N,
        0
    end.

-spec orelse_escape_eval(integer()) -> integer().
orelse_escape_eval(N) ->
    R = maybe
            true ?= flag() orelse N,
            0
        end,
    R.
