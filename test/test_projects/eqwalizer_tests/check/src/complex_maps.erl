%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(complex_maps).

-compile([export_all, nowarn_export_all]).

-spec downcast_neg1(#{a => b, c => d}) -> #{a => b}.
downcast_neg1(X) -> X.

-spec downcast_ok1(#{a => b, c => d}) -> #{a => b, atom() => atom()}.
downcast_ok1(X) -> X.

-spec downcast_neg2(#{a => b, c => d}) -> #{a => b, atom() => integer()}.
downcast_neg2(X) -> X.

-spec upcast_neg1(#{a => b, atom() => atom()}) -> #{a => b, c => d}.
upcast_neg1(X) -> X.

-spec upcast_neg2(#{a => b, atom() => integer()}) -> #{a => b, c => d, atom() => integer()}.
upcast_neg2(X) -> X.

-spec upcast_ok1(#{a := b, atom() => atom()}) -> #{a => b, c => atom(), atom() => atom()}.
upcast_ok1(X) -> X.

-spec upcast_ok2(#{a => b, c => d, e => 3}) -> #{a => b, atom() => atom() | integer()}.
upcast_ok2(X) -> X.

-spec upcast_ok3(#{a := b, integer() => integer()}) -> #{atom() | integer() => atom() | integer()}.
upcast_ok3(X) -> X.

-spec upcast_ok4(#{a => b, atom() => atom()}) -> #{a => b, {ok, k} => v, atom() => atom()}.
upcast_ok4(X) -> X.

-spec downcast_neg3(#{a => b, atom() => atom()}) -> #{a => b}.
downcast_neg3(X) -> X.

-spec downcast_dyn_ok(#{a => b, eqwalizer:dynamic(atom()) => eqwalizer:dynamic(atom())}) -> #{a => b}.
downcast_dyn_ok(X) -> X.

-spec downcast_dyn_neg_1(#{a => b, eqwalizer:dynamic(atom()) => atom()}) -> #{a => b}.
downcast_dyn_neg_1(X) -> X.

-spec downcast_dyn_neg_2(#{a => b, atom() => eqwalizer:dynamic(atom())}) -> #{a => b}.
downcast_dyn_neg_2(X) -> X.

-spec upcast_dyn_ok(#{a => b, atom() => eqwalizer:dynamic(atom())}) -> #{a => b, c => d, atom() => atom()}.
upcast_dyn_ok(X) -> X.

-spec tuple_key_get_ok(#{a => b, {ok, {a, b}} := v}) -> v.
tuple_key_get_ok(#{{ok, {a, b}} := V}) -> V.

-spec tuple_key_get_neg(#{a => b, {ok, {a, b}} := v}) -> err.
tuple_key_get_neg(#{{ok, {a, b}} := V}) -> V.

-spec tuple_key_maps_get_ok(#{a => b, {ok, {a, b}} := v}) -> v.
tuple_key_maps_get_ok(M) -> maps:get({ok, {a, b}}, M).

-spec tuple_key_maps_get_neg(#{a => b, {ok, {a, b}} := v}) -> err.
tuple_key_maps_get_neg(M) -> maps:get({ok, {a, b}}, M).

-spec get_default_ok1(#{a => b, atom() => integer()}) -> b.
get_default_ok1(#{a := V}) -> V.

-spec get_default_ok2(#{a => b, atom() => integer()}) -> integer().
get_default_ok2(#{def := V}) -> V.

-spec get_default_neg1(#{a => b, atom() => integer()}) -> integer().
get_default_neg1(#{a := V}) -> V.

-spec get_default_neg2(#{a => b, atom() => integer()}) -> b.
get_default_neg2(#{def := V}) -> V.

-spec match_required_1_neg(#{a := integer(), atom() => dynamic()}) -> ok.
match_required_1_neg(#{b := _V} = M) -> M.

-spec compat_default_1_neg(#{atom() => binary()}) -> #{a => binary(), atom() => atom()}.
compat_default_1_neg(X) -> X.

-spec compat_default_2_neg(#{a := b, atom() => binary()}) -> #{a => b, {c, d} => atom() | binary(), term() => atom()}.
compat_default_2_neg(X) -> X.

-spec compat_default_3_neg(#{a := b, atom() => binary()}) -> #{a => b, {c, d} => atom() | binary(), atom() => atom()}.
compat_default_3_neg(X) -> X.

-spec dyn_domain_coerce_neg(#{dynamic() => atom()}) -> #{dynamic() => binary()}.
dyn_domain_coerce_neg(X) -> X.

-spec dyn_domain_coerce(#{dynamic() => ok}) -> #{atom() => atom()}.
dyn_domain_coerce(X) -> X.

-spec gradual_guarantee_1(#{dynamic() => binary()}) -> #{a => binary(), b => binary(), c => atom()}.
gradual_guarantee_1(X) -> X.

-type counters() :: #{get := integer(), post := integer()}.

-spec update_counters1(counters(), post | get) -> counters().
update_counters1(Counters, Method) ->
    Counters#{Method := maps:get(Method, Counters, 0) + 1}.
