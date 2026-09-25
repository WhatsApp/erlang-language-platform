%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(maybe_escape).
-feature(maybe_expr, enable).

-compile([export_all, nowarn_export_all]).

%% Without an `else`, a `?=` that does not match makes the whole block evaluate
%% to the value that failed to match, so `true ?= <test>` lets `false` escape
%% and the block is not just `integer()`.

-spec guard_escape_check(term()) -> integer().
guard_escape_check(X) ->
    maybe
        true ?= is_atom(X),
        1
    end.

-spec guard_escape_eval(term()) -> integer().
guard_escape_eval(X) ->
    R = maybe
            true ?= is_atom(X),
            1
        end,
    R.
