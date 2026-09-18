%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(lambda_call_env).

-compile([export_all, nowarn_export_all]).

%% An immediately applied lambda taking no arguments runs unconditionally, so
%% refinements made in its body hold afterwards. The operand of `not` is checked
%% rather than inferred and its environment is threaded out, which is what puts
%% the same call on both paths below.

-spec lambda_env_check(integer() | atom()) -> integer().
lambda_env_check(X) ->
    _ = not ((fun() -> true = is_integer(X) end)()),
    X.

-spec lambda_env_eval(integer() | atom()) -> integer().
lambda_env_eval(X) ->
    _ = (fun() -> true = is_integer(X) end)(),
    X.

%% Once the lambda takes arguments the refinement no longer escapes, on either
%% path, so this one stays an error.
-spec lambda_env_with_args_check(integer() | atom()) -> integer().
lambda_env_with_args_check(X) ->
    _ = not ((fun(_) -> true = is_integer(X) end)(ok)),
    X.

-spec lambda_env_with_args_eval(integer() | atom()) -> integer().
lambda_env_with_args_eval(X) ->
    _ = (fun(_) -> true = is_integer(X) end)(ok),
    X.
