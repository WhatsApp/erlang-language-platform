%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_named_lambda_call).

-compile([export_all, nowarn_export_all]).

%% A named fun applied on the spot is given all-dynamic parameters whichever way
%% the call itself is typed, so it is a lambda without context on both paths.
%% Only the expected result type differs between them.

-spec named_lambda_check() -> integer().
named_lambda_check() ->
    (fun _Loop(X) -> X end)(1).

-spec named_lambda_eval() -> integer().
named_lambda_eval() ->
    R = (fun _Loop(X) -> X end)(1),
    R.
