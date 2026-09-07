%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(inter).

-compile([export_all, nowarn_export_all]).

-type i(T1, T2) :: eqwalizer:inter(T1, T2).

-type ma1() :: #{a := atom(), term() => term()}.
-type mb1() :: #{b := binary(), term() => term()}.

-type ma2() :: #{a => atom(), term() => term()}.
-type mb2() :: #{b => binary(), term() => term()}.

-spec map_inter1() -> i(ma1(), mb1()).
map_inter1() -> #{a => a, b => ~""}.

-spec map_inter1_neg1() -> i(ma1(), mb1()).
map_inter1_neg1() -> #{a => a}.

-spec map_inter2() -> i(ma2(), mb2()).
map_inter2() -> #{a => a}.

-spec map_inter2_neg1() -> i(ma2(), mb2()).
map_inter2_neg1() -> #{a => ~""}.

% "bounded id"
-spec b_id(i(A, atom() | binary())) -> i(A, atom() | binary()).
b_id(X) -> X.

-spec b_id_app1(atom()) -> atom().
b_id_app1(A) -> b_id(A).

-spec b_id_app2(binary()) -> binary().
b_id_app2(B) -> b_id(B).

-spec b_id_app3(foo | bar | binary()) -> foo | bar | binary().
b_id_app3(A) -> b_id(A).

-spec b_id_app_neg1(atom() | binary()) -> atom().
b_id_app_neg1(AB) -> b_id(AB).

%% Narrowing operations over intersection types (positive checks).
%% `&` is not Erlang type syntax; intersection is written via the `i/2` alias above.
-type ta() :: {a, atom()}.
-type tb() :: {atom(), foo | bar}.
%% ta() & tb() narrows to {a, foo | bar}.

%% asTupleType (tuple pattern) — element 1 is 'a' in both conjuncts
-spec tuple_inter_pat(i(ta(), tb())) -> a.
tuple_inter_pat({X, _}) -> X.

%% getTupleElement (erlang:element/2 with a literal index)
-spec tuple_inter_element(i(ta(), tb())) -> a.
tuple_inter_element(T) -> element(1, T).

%% getAllTupleElements (erlang:element/2 with a non-literal index)
-spec tuple_inter_all_elems(i(ta(), tb()), integer()) -> atom().
tuple_inter_all_elems(T, N) -> element(N, T).

%% setTupleElement (erlang:setelement/3)
-spec tuple_inter_set(i(ta(), tb())) -> tuple().
tuple_inter_set(T) -> setelement(1, T, a).

%% asMapTypes + asKeys (maps:get/2 over an intersection of maps)
-spec map_inter_get(i(ma1(), mb1())) -> atom().
map_inter_get(M) -> maps:get(a, M).

%% asMapTypes (map update over an intersection of maps)
-spec map_inter_put(i(ma1(), mb1())) -> i(ma1(), mb1()).
map_inter_put(M) -> M#{a => x}.

%% asMapOrIterTypes (comprehension generator over an intersection of maps)
-spec map_inter_comp(i(ma2(), mb2())) -> [term()].
map_inter_comp(M) -> [V || _ := V <- M].

%% asFunTypes + dyn-call of an intersected function type
-type fa() :: fun((atom()) -> atom()).
-type fb() :: fun((atom()) -> foo | bar).

-spec fun_inter_call(i(fa(), fb()), atom()) -> foo | bar.
fun_inter_call(F, X) -> F(X).

%% asListType / extractListElem — element type is the meet of the conjuncts'
-spec list_inter_elem(i([atom()], [foo | bar | baz])) -> [foo | bar | baz].
list_inter_elem(L) -> [X || X <- L].

%% Occurrence typing turns `Box :: box(T) | T` into `#box{} & T | box(T)`.
%% That has to be accepted both by subtyping and by constraint generation,
%% i.e. as an argument of a polymorphic function.
-record(box, {v :: fun(() -> eqwalizer:dynamic())}).
-type box(T) :: #box{v :: fun(() -> T)}.

-spec unbox(box(T)) -> T.
unbox(#box{v = V}) -> V().

-spec inter_rec_sub(i(#box{}, T) | box(T)) -> box(T).
inter_rec_sub(Box) -> Box.

-spec inter_rec_poly_call(i(#box{}, T) | box(T)) -> T.
inter_rec_poly_call(Box) -> unbox(Box).
