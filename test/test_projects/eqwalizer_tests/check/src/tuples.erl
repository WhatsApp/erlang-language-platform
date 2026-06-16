%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(tuples).

-compile([export_all, nowarn_export_all]).

-record(rec, {a :: atom(), b :: binary()}).

-spec test1(tuple()) -> integer().
test1(Tuple) ->
    case Tuple of
        {_} -> ok
    end,
    eqwalizer:reveal_type(Tuple),
    {Z} = Tuple,
    Z.

-spec test2_neg(term()) -> {atom(), binary()}.
test2_neg(Term) ->
    case Term of
        T when is_tuple(T) -> T
    end.

-spec setelement1({atom(), binary()}) -> {binary(), binary()}.
setelement1(AB) -> setelement(1, AB, ~"").

-spec setelement_neg({atom(), binary()}) -> {atom(), binary()}.
setelement_neg(AB) -> setelement(1, AB, ~"").

-spec update_rec(#rec{}) -> #rec{}.
update_rec(Rec) -> setelement(2, Rec, red).

-spec update_rec_neg(#rec{}) -> #rec{}.
update_rec_neg(Rec) -> setelement(2, Rec, ~"red").
