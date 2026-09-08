%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(match_normalization).

-compile([export_all, nowarn_export_all]).

%% `true = <test>` and nested matches are given special treatment before the
%% generic `Pat = Expr` case is reached. The operand of `not` is checked rather
%% than inferred and its environment is threaded out, so each pair below runs
%% the same match down the checked and the inferred path.

-spec guard_refine_check(integer() | atom()) -> integer().
guard_refine_check(X) ->
  _ = not (true = is_integer(X)),
  X.

-spec guard_refine_eval(integer() | atom()) -> integer().
guard_refine_eval(X) ->
  true = is_integer(X),
  X.

-spec nested_match_check(integer() | atom(), boolean()) -> true.
nested_match_check(X, B) ->
  _ = not (true = B = is_atom(X)),
  B.

-spec nested_match_eval(integer() | atom(), boolean()) -> true.
nested_match_eval(X, B) ->
  true = B = is_atom(X),
  B.
