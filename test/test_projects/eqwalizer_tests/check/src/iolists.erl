%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(iolists).

-compile([export_all, nowarn_export_all]).

-spec mk_io_list1(
    byte() | binary() | iodata()
) -> iodata().
mk_io_list1(X) ->
    [X].

-spec first(iodata()) ->
byte() | binary() | iodata().
first(IoList)
    when is_binary(IoList) -> IoList;
first([H|_]) -> H.

-spec refine_as_list(iodata()) ->
byte() | binary() | iodata().
refine_as_list(IoList)
    when is_list(IoList) ->
    IoList;
refine_as_list(IoList)
    when is_binary(IoList) ->
    binary_to_list(IoList).

-spec refine1([term()], iodata()) ->
[byte() | binary() | iodata()].
refine1(X, X) -> X.

-spec refine2(iodata(), [term()]) ->
    [byte() | binary() | iodata()].
refine2(X, X) -> X.

-spec refine3(term(), iodata()) ->
    binary() | [byte() | binary() | iolist()].
refine3(X, X) -> X.

-spec refine4(iodata(), term()) ->
    binary() | [byte() | binary() | iolist()].
refine4(X, X) -> X.

-spec refine5(
    iodata(), [atom() | binary()]
) -> [binary()].
refine5(X, X) -> X.

-spec refine6_neg(
    iodata(), [atom() | binary()]
) -> [atom()].
refine6_neg(X, X) -> X.

-spec refine_to_empty1(
    iodata(), [atom()]
) -> [].
refine_to_empty1(X, X) -> X.

-spec refine_to_empty2(
    [atom()], iodata()
) -> [].
refine_to_empty2(X, X) -> X.

-spec head_or([A], A) -> A.
head_or([A], _) -> A;
head_or([_], A) -> A.

-spec io_list_head(iodata()) ->
    binary() | iodata() | number().
io_list_head(X) when is_list(X)
    -> head_or(X, X).

-spec ioio(iodata(), A) -> A.
ioio(_, A) ->  A.

-spec test() -> atom().
test() -> ioio([<<>>], ok).

-spec test2_neg(iodata()) -> wrong_ret.
test2_neg(X) -> X.
