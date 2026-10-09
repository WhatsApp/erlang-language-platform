%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(layered_overrides).

-export([
    bundled_spec/1,
    project_spec_wins_neg/0,
    bundled_local_type_neg/0,
    project_plain_spec_wins_neg/0
]).

% erpc:call/2 is only overridden by the bundled eqwalizer_specs
-spec bundled_spec(node()) -> atom().
bundled_spec(Node) ->
    Res = erpc:call(Node, fun() -> ok end),
    eqwalizer:reveal_type(Res),
    Res.

% The project's 'lists:delete'(T, [T]) replaces the bundled
% 'lists:delete'(term(), [T])
-spec project_spec_wins_neg() -> [integer()].
project_spec_wins_neg() ->
    lists:delete(a, [1]).

% The bundled 'argparse:run'/3 refers to a type local to the bundled
% eqwalizer_specs, which the project's copy does not declare
-spec bundled_local_type_neg() -> term().
bundled_local_type_neg() ->
    argparse:run(["arg"], #{}, #{progname => 42}).

% The project's plain 'filename:split'/1 replaces the bundled overloaded one
-spec project_plain_spec_wins_neg() -> [binary()].
project_plain_spec_wins_neg() ->
    filename:split(<<"a/b">>).
