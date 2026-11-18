%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(otp29). % @OTP29Only

-compile([export_all, nowarn_export_all]).

-if(?OTP_RELEASE >= 29).

-spec int_range_guard(term()) -> integer().
int_range_guard(X) when is_integer(X, 0, 10) -> X;
int_range_guard(_) -> 0.

-spec int_range_union(integer() | atom()) -> integer().
int_range_union(X) when is_integer(X, -5, 5) -> X;
int_range_union(_) -> 0.

-spec int_range_bounds(term(), term(), term()) ->
    {integer(), integer(), integer()}.
int_range_bounds(X, Min, Max) when is_integer(X, Min, Max) -> {X, Min, Max};
int_range_bounds(_, _, _) -> {0, 0, 0}.

-spec int_range_bounds_else_neg(term(), term()) -> integer().
int_range_bounds_else_neg(X, Max) when is_integer(X, 0, Max) -> Max;
int_range_bounds_else_neg(_, Max) -> Max.

-spec int_range_else_neg(integer() | atom()) -> atom().
int_range_else_neg(X) when is_integer(X, 0, 10) -> ok;
int_range_else_neg(X) -> X.

-spec int_range_not_neg(integer() | atom()) -> atom().
int_range_not_neg(X) when not is_integer(X, 0, 10) -> X;
int_range_not_neg(_) -> ok.

-spec int_range_if(term()) -> integer().
int_range_if(X) ->
    if
        is_integer(X, 1, 9) -> X;
        true -> 0
    end.

-spec int_range_case(term()) -> integer().
int_range_case(X) ->
    case is_integer(X, 1, 9) of
        true -> X;
        false -> 0
    end.

-spec int_range_andalso(term()) -> boolean().
int_range_andalso(X) ->
    is_integer(X, 0, 255) andalso is_even(X).

-spec int_range_assert(term()) -> integer().
int_range_assert(X) ->
    is_integer(X, 0, 255) orelse error(badarg),
    X.

-spec int_range_filter([term()]) -> [integer()].
int_range_filter(L) ->
    [X || X <- L, is_integer(X, 0, 255)].

-spec int_range_call(term()) -> boolean().
int_range_call(X) ->
    is_integer(X, 0, 10).

-spec int_range_bad_bound_neg(term()) -> boolean().
int_range_bad_bound_neg(X) ->
    is_integer(X, 0, infinity).

-spec is_even(integer()) -> boolean().
is_even(N) -> N rem 2 =:= 0.

-spec int_range_not_else(term()) -> integer().
int_range_not_else(X) when not is_integer(X, 0, 10) -> 0;
int_range_not_else(X) -> X.

-spec int_range_not_else_negative(term()) -> integer().
int_range_not_else_negative(X) when not is_integer(X, -5, 5) -> 0;
int_range_not_else_negative(X) -> X.

-spec int_range_if_not(term()) -> integer().
int_range_if_not(X) ->
    if
        not is_integer(X, 0, 10) -> 0;
        true -> X
    end.

-spec int_range_orelse_else(integer() | atom()) -> integer().
int_range_orelse_else(X) when is_integer(X, 0, 10) orelse is_atom(X) -> 0;
int_range_orelse_else(X) -> X.

-spec int_range_bound_else_neg(term(), term()) -> integer().
int_range_bound_else_neg(X, Min) when is_integer(X, Min, 10) -> Min;
int_range_bound_else_neg(_, Min) -> Min.

-spec int_range_match(term()) -> integer().
int_range_match(X) ->
    true = is_integer(X, 0, 10),
    X.

-spec int_range_qualified(term()) -> integer().
int_range_qualified(X) when erlang:is_integer(X, 0, 10) -> X;
int_range_qualified(_) -> 0.

-spec mv_list([{atom(), integer()}]) -> [atom() | integer()].
mv_list(L) -> [K, V || {K, V} <- L].

-spec mv_list_neg([{atom(), integer()}]) -> [atom()].
mv_list_neg(L) -> [K, V || {K, V} <- L].

-spec mv_list_filter([term()]) -> [integer()].
mv_list_filter(L) -> [X, -X || X <- L, is_integer(X)].

-spec mv_list_elab_neg([{atom(), atom()}]) -> [atom() | number()].
mv_list_elab_neg(L) -> [K, V + 1 || {K, V} <- L].

-spec mv_list_zip([atom()], [integer()]) -> [atom() | integer()].
mv_list_zip(As, Is) -> [A, I || A <:- As && I <:- Is].

-spec mv_map([{atom(), integer()}]) -> #{atom() | integer() => atom() | integer()}.
mv_map(L) -> #{K => V, V => K || {K, V} <- L}.

-spec mv_map_neg([{atom(), integer()}]) -> #{atom() => integer()}.
mv_map_neg(L) -> #{K => V, V => K || {K, V} <- L}.

-spec mv_map_elab_neg([{atom(), atom()}]) -> #{atom() => atom() | number()}.
mv_map_elab_neg(L) -> #{K => V, V => V + 1 || {K, V} <- L}.

-spec mv_map_gen(#{atom() => integer()}) -> #{atom() | integer() => atom() | integer()}.
mv_map_gen(M) -> #{K => V, V => K || K := V <- M}.

-spec mv_map_gen_neg(#{atom() => integer()}) -> #{atom() => integer()}.
mv_map_gen_neg(M) -> #{K => V, V => K || K := V <- M}.

-spec mv_map_gen_strict(#{atom() => integer()}) -> #{atom() | integer() => atom() | integer()}.
mv_map_gen_strict(M) -> #{K => V, V => K || K := V <:- M}.

-spec mv_map_gen_strict_neg(#{atom() => integer()}) -> #{atom() => integer()}.
mv_map_gen_strict_neg(M) -> #{K => V, V => K || K := V <:- M}.

-spec mv_map_gen_elab_neg(#{atom() => atom()}) -> #{atom() => atom() | number()}.
mv_map_gen_elab_neg(M) -> #{K => V, V => V + 1 || K := V <- M}.

-spec mv_map_gen_filter(#{atom() | binary() => integer()}) -> #{atom() | integer() => atom() | integer()}.
mv_map_gen_filter(M) -> #{K => V, V => K || K := V <- M, is_atom(K)}.

-endif.
