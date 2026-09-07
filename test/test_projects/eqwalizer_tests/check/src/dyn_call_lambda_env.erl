%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dyn_call_lambda_env).

-compile([export_all, nowarn_export_all]).

%% `X = 1` refines X to integer() while the arguments are elaborated, and a
%% lambda argument is type-checked again when the call is applied. That second
%% pass must see the refinement, in check position just as in eval position.

-spec lambda_arg_check(
    fun((integer(), fun((integer()) -> integer())) -> integer()),
    integer() | atom()
) -> integer().
lambda_arg_check(F, X) ->
  F(X = 1, fun(Y) -> Y + X end).

-spec lambda_arg_eval(
    fun((integer(), fun((integer()) -> integer())) -> integer()),
    integer() | atom()
) -> integer().
lambda_arg_eval(F, X) ->
  Res = F(X = 1, fun(Y) -> Y + X end),
  Res.
