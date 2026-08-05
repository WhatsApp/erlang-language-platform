%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(constraints_ordering).

-compile([export_all, nowarn_export_all]).

%% Solution ordering in local inference:
%% a parameter typed as a union of a type variable and a structural
%% type over the same variable admits several solutions for an
%% argument matching both members; the minimal solution must be
%% preferred.

-spec branch(T | [T]) -> T.
branch(_) -> throw(not_implemented).

%% T = 'a' (minimal), not 'a' | ['a']
-spec branch_atom_print(a | [a]) -> err.
branch_atom_print(X) ->
  eqwalizer:reveal_type(branch(X)).

%% forwarding call: the minimal solution makes the result T, no error
-spec branch_fwd(T | [T]) -> T.
branch_fwd(X) ->
  branch(X).
