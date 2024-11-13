%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(kp_02).
-compile([export_all, nowarn_export_all]).

-spec pure_fun(F) -> F.
pure_fun(F) -> F.

-spec transform(fun((T1) -> Ret), T1) -> Ret.
transform(F, A) ->
    F(A).

-spec id(X) -> X.
id(X) -> X.

-spec use_transform1(X) -> X when X :: number().
use_transform1(A) ->
    % should type-check similar to use_transform2, but doesn't.
    % the reason is `pure_fun` wrapper and how we handle
    % "lambda args" and "non-lambda args"
    Res = transform(pure_fun(fun id/1), A),
    Res.

-spec use_transform2(X) -> X when X :: number().
use_transform2(A) ->
    Res = transform(fun id/1, A),
    Res.
