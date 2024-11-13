%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(kp_01).
-compile([export_all, nowarn_export_all]).

-type pure(F) :: F.

-spec transform1(pure(fun((T1) -> Ret)), T1) -> Ret.
transform1(F, A) ->
    F(A).

-spec transform2(fun((T1) -> Ret), T1) -> Ret.
transform2(F, A) ->
    F(A).

-spec id(X) -> X.
id(X) -> X.

-spec use_transform1(X) -> X when X :: atom().
use_transform1(A) ->
    % should type-check - similar to use_transform2
    % the reason is that transform1 accepts pure(fun((T1) -> Ret))
    % and something goes wrong with alias unfolding
    Res = transform1(fun id/1, A),
    Res.

-spec use_transform2(X) -> X when X :: atom().
use_transform2(A) ->
    Res = transform2(fun id/1, A),
    Res.
