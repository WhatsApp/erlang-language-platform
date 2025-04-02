%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(otp28).

-compile([export_all, nowarn_export_all]).

-nominal foo() :: ok | error.

-spec mk_nominal() -> foo().
mk_nominal() -> ok.

-spec mk_nominal_neg() -> foo().
mk_nominal_neg() -> warning.

-spec strict_comp_1
    ([{atom()} | {atom(), number()}]) ->
    [atom()].
strict_comp_1(L) ->
    [A || {A} <:- L].

-spec strict_comp_2_neg
    ([{atom()} | {atom(), number()}]) ->
    [atom()].
strict_comp_2_neg(L) ->
    [N || {_, N} <:- L].

-spec strict_comp_3
    (#{atom() => {ok, pid()} | {error, binary()}}) ->
    [pid()].
strict_comp_3(L) ->
    [Pid || _ := {ok, Pid} <:- L].

-spec strict_comp_4_neg
    (#{atom() => {ok, pid()} | {error, binary()}}) ->
    [pid()].
strict_comp_4_neg(L) ->
    [Bin || _ := {error, Bin} <:- L].

-spec strict_comp_5
    ([{atom(), {ok, pid()} | {error, binary()}}]) ->
    #{atom() => pid()}.
strict_comp_5(L) ->
    #{K => Pid || {K, {ok, Pid}} <:- L}.

-spec strict_comp_6_neg
    ([{atom(), {ok, binary()} | {error, pid()}}]) ->
    [#{atom() => pid()}].
strict_comp_6_neg(L) ->
    #{K => Bin || {K, {ok, Bin}} <:- L}.
