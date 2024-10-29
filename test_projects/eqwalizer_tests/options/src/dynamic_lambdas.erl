%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_lambdas).
-compile([export_all, nowarn_export_all]).

-spec dyn_lambda1() -> fun().
dyn_lambda1() ->
    Fun = fun(X) -> X end,
    Fun.

-spec dyn_lambda2() -> [atom()].
dyn_lambda2() ->
    Fun = fun(X) -> X end,
    L = [foo, bar],
    lists:map(Fun, L).

-spec dyn_lambda3() -> list().
dyn_lambda3() ->
    List = [
        fun(X) -> X end,
        fun(#{}=M) -> M#{a => b} end
    ],
    List.

-spec dyn_lambda4() -> map().
dyn_lambda4() ->
    Map = #{
        a => fun(X) -> X end,
        b => fun(#{}=M) -> M#{a => b} end
    },
    Map.

-spec non_dyn_lambda1() -> [atom()].
non_dyn_lambda1() ->
    L = [foo, bar],
    lists:map(fun(X) -> X end, L).

-spec non_dyn_lambda2() -> [atom()].
non_dyn_lambda2() ->
    L = [true, false],
    lists:filtermap(fun(X) -> X end, L).
