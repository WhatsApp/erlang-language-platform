%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dyn_pat).

-compile([export_all, nowarn_export_all]).

-include_lib("eqwalizer/include/eqwalizer.hrl").

-spec fun_clause_neg(integer()) -> atom().
fun_clause_neg(X) -> X.

-spec fun_clause_pos(integer()) -> atom().
fun_clause_pos(X = ?dyn_pattern) -> X.

-spec fun_clause_tuple_pos({integer(), binary()}) -> {atom(), atom()}.
fun_clause_tuple_pos({A, B} = ?dyn_pattern) -> {A, B}.

-spec fun_clause_multi_pos(integer(), integer()) -> {atom(), integer()}.
fun_clause_multi_pos(X = ?dyn_pattern, Y) -> {X, Y}.

-spec fun_clause_multi_neg(integer(), integer()) -> {atom(), atom()}.
fun_clause_multi_neg(X = ?dyn_pattern, Y) -> {X, Y}.

-spec case_clause_neg(integer()) -> atom().
case_clause_neg(I) ->
  case I of
    X -> X
  end.

-spec case_clause_pos(integer()) -> atom().
case_clause_pos(I) ->
  case I of
    X = ?dyn_pattern -> X
  end.

-spec lambda_neg() -> fun((integer()) -> atom()).
lambda_neg() -> fun(X) -> X end.

-spec lambda_pos() -> fun((integer()) -> atom()).
lambda_pos() -> fun(X = ?dyn_pattern) -> X end.

-spec receive_pos() -> atom().
receive_pos() ->
  receive
    X = ?dyn_pattern -> X
  end.
