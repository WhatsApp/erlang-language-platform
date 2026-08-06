%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_constraints).

-compile([export_all, nowarn_export_all]).

%% Local inference: a type variable constrained both by a
%% concrete argument and by an argument carrying dynamic()
%% keeps dynamic() as a lower bound.

-spec cons(T, [T]) -> [T].
cons(H, T) -> [H | T].

-spec cons_dyn_print([dynamic()]) -> err.
cons_dyn_print(L) ->
  eqwalizer:reveal_type(cons(ok, L)).

%% A generic parameter typed as a union of the variable and a
%% function over the variable: the function alternative must
%% not be absorbed into the variable's solution.

-spec expect(T | fun((T | dynamic()) -> boolean())) -> T.
expect(_E) -> throw(not_implemented).

-spec expect_atom_print(
    atom() | fun((atom() | dynamic()) -> boolean())
) -> err.
expect_atom_print(E) ->
  eqwalizer:reveal_type(expect(E)).

-spec expect_fwd_neg(
    T | fun((T | dynamic()) -> boolean())
) -> T.
expect_fwd_neg(E) ->
  expect(E).

%% Dynamic lower bounds reaching a map type through
%% maps:from_list/1 and maps:merge/2: the merged map gains a
%% default association.

-type kvs(K, V) :: [{K, V}].

-spec set(K, V, kvs(K, V)) -> kvs(K, V).
set(K, V, KVs) -> [{K, V} | KVs].

-spec set_dyn_print(
    kvs(dynamic(), dynamic())
) -> err.
set_dyn_print(KVs) ->
  eqwalizer:reveal_type(set(b, 3, KVs)).

-spec from_list_dyn_print(
    kvs(dynamic(), dynamic())
) -> err.
from_list_dyn_print(KVs) ->
  eqwalizer:reveal_type(
    maps:from_list(set(b, 3, KVs))
  ).

-spec merge_dyn_neg(
    kvs(dynamic(), dynamic())
) -> #{a := number(), b := number()}.
merge_dyn_neg(KVs) ->
  maps:merge(
    #{a => 1, b => 2},
    maps:from_list(set(b, 3, KVs))
  ).
