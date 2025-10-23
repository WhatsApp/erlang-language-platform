%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater).

-compile([export_all, nowarn_export_all]).

-spec occ01
  (a | b) -> b.
occ01(Arg) ->
  case Arg of
    a -> b;
    B -> B
  end.

-spec occ01_elab
  (a | b) -> b.
occ01_elab(Arg) ->
  Res = case Arg of
    a -> b;
    B -> B
  end,
  Res.

-spec occ01_cl
  (a | b) -> b.
occ01_cl(a) -> b;
occ01_cl(B) -> B.

-spec occ02
  (a | {b, integer()}) -> integer().
occ02(Arg) ->
  case Arg of
    a -> 0;
    {_, I} -> I
  end.

-spec occ02_cl
  (a | {b, integer()}) -> integer().
occ02_cl(a) -> 0;
occ02_cl({_, I}) -> I.

-spec occ03
  ({a, atom()} | {b, binary()}) ->
  binary().
occ03(Arg) ->
  case Arg of
    {a, A} -> atom_to_binary(A);
    {_, B} -> B
  end.

-spec occ03_cl
  ({a, atom()} | {b, binary()}) ->
  binary().
occ03_cl({a, A}) -> atom_to_binary(A);
occ03_cl({_, B}) -> B.

-spec occ04(atom() | binary())
    -> binary().
occ04(Arg) ->
  case Arg of
    A when is_atom(A) ->
      atom_to_binary(A);
    B ->
      B
  end.

-spec occ04_cl
  (atom() | binary())
    -> binary().
occ04_cl(A) when is_atom(A) ->
  atom_to_binary(A);
occ04_cl(B) ->
  B.

-spec occ04_if(atom() | binary())
    -> binary().
occ04_if(A) ->
  if
    is_atom(A) -> atom_to_binary(A);
    true -> A
  end.

-spec occ04_if_elab(atom() | binary())
    -> binary().
occ04_if_elab(A) ->
  Res = if
    is_atom(A) -> atom_to_binary(A);
    true -> A
  end,
  Res.

-spec occ05_1
  (atom() | integer() | binary())
    -> binary().
occ05_1(Arg) ->
  case Arg of
    A when is_atom(A) or
           is_integer(A) -> <<>>;
    B -> B
  end.

-spec occ05_1_cl
  (atom() | integer() | binary())
    -> binary().
occ05_1_cl(A)
  when is_atom(A) or
       is_integer(A) -> <<>>;
occ05_1_cl(B) -> B.

-spec occ05_1_if
  (atom() | integer() | binary())
    -> binary().
occ05_1_if(A) ->
  if
    is_atom(A) or
    is_integer(A) -> <<>>;
    true -> A
  end.

-spec occ05_2
  (atom() | integer() | binary())
    -> binary().
occ05_2(Arg) ->
  case Arg of
    A when is_atom(A); is_integer(A) ->
      <<>>;
    B ->
      B
  end.

-spec occ05_2_cl
  (atom() | integer() | binary())
    -> binary().
occ05_2_cl(A)
  when is_atom(A); is_integer(A) ->
  <<>>;
occ05_2_cl(B) -> B.

-spec occ05_2_if
  (atom() | integer() | binary())
    -> binary().
occ05_2_if(A) ->
  if
    is_atom(A); is_integer(A) ->
      <<>>;
    true ->
      A
  end.

-spec occ05_3_neg
  (atom() | integer() | binary())
    -> binary().
occ05_3_neg(Arg) ->
  case Arg of
    A when is_atom(A);
      is_integer(A), A > 0 ->
      <<>>;
    B ->
      B
  end.

-spec occ05_3_neg_cl
  (atom() | integer() | binary())
    -> binary().
occ05_3_neg_cl(A)
  when is_atom(A);
       is_integer(A), A > 0 ->
    <<>>;
occ05_3_neg_cl(B) -> B.

-spec occ05_3_neg_if
  (atom() | integer() | binary())
    -> binary().
occ05_3_neg_if(A) ->
  if
    is_atom(A);
    is_integer(A), A > 0 ->
      <<>>;
    true ->
      A
  end.

-spec occ06
  (atom() | integer() | binary())
    -> binary().
occ06(Arg) ->
  case Arg of
    A when not is_binary(A) ->
      <<>>;
    B ->
      B
  end.

-spec occ06_cl
  (atom() | integer() | binary())
    -> binary().
occ06_cl(A)
  when not is_binary(A) ->
    <<>>;
occ06_cl(B) ->
  B.

-spec occ06_if
  (atom() | integer() | binary())
    -> binary().
occ06_if(A) ->
  if
    not is_binary(A) ->
      <<>>;
    true ->
      A
  end.

-spec occ07
  (atom() | binary(),
    number() | list()) ->
  binary() | number().
occ07(X, _) when not is_atom(X) -> X;
occ07(_, Y) when is_list(Y) -> 0;
occ07(_, Y) -> Y.

%%% getting none()

-spec occ08(atom()) -> none().
occ08(A) when is_number(A) -> A.

-spec occ09(atom()) -> boolean() | undef.
occ09(X) when is_boolean(X) -> X;
occ09(_) -> undef.

-spec produce() -> atom() | binary().
produce() -> error(not_implemented).

-spec close() -> ok.
close() -> ok.

-spec try_of_01() -> binary().
try_of_01() ->
  try
    produce()
  of
    A when is_atom(A) ->
      atom_to_binary(A);
    B -> B
  catch
    _ -> <<>>
  after
    close()
  end.

-spec try_of_01_elab() -> binary().
try_of_01_elab() ->
  Res = try
    produce()
  of
    A when is_atom(A) ->
      atom_to_binary(A);
    B -> B
  catch
    _ -> <<>>
  after
    close()
  end,
  Res.

-spec lambda_call(X :: atom() | binary())
  -> binary().
lambda_call(X) ->
  (fun
    (A) when is_atom(A) ->
      atom_to_binary(A);
    (B) -> B
  end)(X).

-spec lambda_call_elab
(X :: atom() | binary()) -> binary().
lambda_call_elab(X) ->
  Res =
  (fun
     (A) when is_atom(A) ->
       atom_to_binary(A);
     (B) -> B
  end)(X),
  Res.

-spec
lambda1() ->
  fun((atom() | binary())
      -> binary()).
lambda1() ->
  fun
    (A) when is_atom(A) ->
      atom_to_binary(A);
    (B) -> B
  end.

-record(eqwater, {f1, f2}).
-spec occ10(#eqwater{} | {err, term()}) ->
  {ok, term()} | {err, term()}.
occ10(In) ->
  case In of
    #eqwater{} -> {ok, In};
    Other -> Other
  end.

%% important for thrift
%% see D31025723
-record(rec, {
  id1 :: undefined | integer(),
  id2 :: undefined | integer()
}).
-spec collect_ids(#rec{}) ->
  [integer()].
collect_ids(Rec) ->
  Ids0 = [],
  Ids1 = case Id1 = Rec#rec.id1 of
    undefined -> Ids0;
    _ -> [Id1 | Ids0]
  end,
  Ids2 = case Id2 = Rec#rec.id1 of
    undefined -> Ids0;
    _ -> [Id2 | Ids1]
  end,
  Ids2.

-record(a_rec, {
  a :: atom()
}).
-record(ab_rec, {
  ab :: atom() | binary()
}).

-spec occ11(#ab_rec{} | atom())
  -> atom().
occ11(#ab_rec{ab = A})
  when is_atom(A) -> A;
occ11(A) -> A.

-spec occ12(#ab_rec{} | atom())
  -> atom().
occ12(#ab_rec{ab = A}) ->
  if
    is_atom(A) -> A;
    true -> undefined
  end;
occ12(A) -> A.

-spec occ13
(#ab_rec{} | atom(), #a_rec{})
  -> atom().
occ13(#ab_rec{ab = A},
      #a_rec{a = A}) -> A;
occ13(A, _) -> A.

-spec occ14
  (#a_rec{} | atom())
    -> atom().
occ14(#a_rec{a = Z}) -> Z;
occ14(A) -> A.

-spec occ15
  (#a_rec{} | atom(), #ab_rec{})
    -> atom().
occ15(#a_rec{a = Z},
  #ab_rec{}) -> Z;
occ15(A, _) -> A.

-spec occ16
  (#a_rec{} | atom(), #ab_rec{})
    -> atom().
occ16(#a_rec{},
  #ab_rec{}) -> ok;
occ16(A, _) -> A.

-spec occ17(
{ax, atom()} | atom(), {bx, atom()}) -> atom().
occ17({ax, _}, {bx, _}) -> ok;
occ17(A, _) -> A.

% correct work with names and scopes

-spec get_int1(rich_tuple()) -> integer().
get_int1({I, _})
  when is_integer(I) -> I;
get_int1({_, I}) -> I.

-spec get_int2(rich_tuple()) -> integer().
get_int2({I1, _})
  when is_integer(I1) -> I1;
get_int2({_, I2}) -> I2.

-spec get_int3(rich_tuple()) -> integer().
get_int3({A, I})
  when is_atom(A) -> I;
get_int3({I, _}) -> I.

-spec get_int4_neg
  (rich_tuple()) -> integer().
get_int4_neg({A, I})
  when is_atom(A) -> I;
get_int4_neg({_, I}) -> I.

-spec get_int5
  (rich_tuple()) -> none().
get_int5({A, A})
  when is_atom(A) -> A.

-spec get_int6
  (rich_tuple()) -> number().
get_int6({A1, A2})
  when is_atom(A1) and is_atom(A2) -> 1;
get_int6({_, I}) -> I.

-spec get_int7_neg
  (rich_tuple()) -> number().
get_int7_neg({A, A})
  % A is none()
  when is_atom(A) -> 0;
get_int7_neg({_, I}) -> I.

-spec get_int8_neg
  (rich_tuple()) -> number().
get_int8_neg({A, A1})
% A is none()
  when is_atom(A) andalso A == A1 -> 0;
get_int8_neg({_, I}) -> I.

-spec use_num_literals(
  {number(), atom()} |
  {atom(), binary()}
) -> binary().
use_num_literals({3.14, A}) ->
  atom_to_binary(A);
use_num_literals({0, A}) ->
  atom_to_binary(A);
use_num_literals({N, _})
  when is_number(N) -> <<>>;
use_num_literals({_, B}) ->
  B.

-record(a, {id :: atom()}).
-record(b, {id :: atom()}).
-record(c, {id :: atom()}).

-spec ab_b1(#a{} | #b{}) -> #b{}.
ab_b1(A) when is_record(A, a) ->
  #b{id = A#a.id};
ab_b1(B) ->
  B.

-record(union_field, {
  field :: atom() | binary()
}).

-record(union_field2, {
  field :: #a{} | #b{}
}).

-spec record_occ01
(#union_field{}) -> binary().
record_occ01(#union_field{field = B})
  when is_binary(B) -> B;
record_occ01(#union_field{field = A}) ->
  atom_to_binary(A).

-spec record_occ02(#a{} | #b{}) -> #b{}.
record_occ02(#a{id=Id}) ->
  #b{id = Id};
record_occ02(B) ->
  B.

-spec record_occ03
(#union_field2{}) -> #b{}.
record_occ03(#union_field2{field = A})
  when is_record(A, a) ->
  #b{id = A#a.id};
record_occ03(#union_field2{field = B}) ->
  B.

-spec record_occ04
  (#union_field{}) -> binary().
record_occ04(R)
  when is_binary(R#union_field.field) ->
  R#union_field.field;
record_occ04(R) ->
  atom_to_binary(R#union_field.field).

-spec record_occ05_neg
  (#union_field{}) -> binary().
record_occ05_neg(#union_field{field = B})
  when is_binary(B) -> B;
record_occ05_neg(#union_field{field = A}) ->
  A.

-spec record_occ06
  (#union_field{}) -> binary().
record_occ06(#union_field{field = B})
  when is_binary(B) -> B;
record_occ06(R) ->
  atom_to_binary(R#union_field.field).

-spec record_occ07_tuple
  ({union_field, atom()}) -> binary().
record_occ07_tuple({_, A}) -> atom_to_binary(A).

-spec record_occ07
  (#union_field{}) -> binary().
record_occ07(#union_field{field = B})
  when is_binary(B) -> B;
record_occ07(R) ->
  record_occ07_tuple(R).

-spec record_occ08_neg
  (#union_field{}) -> (atom() | binary()).
record_occ08_neg(#union_field{field = A})
  when is_atom(A) -> A;
record_occ08_neg(R) ->
  record_occ07_tuple(R).

-record(union_field3, {
  union :: atom() | binary(),
  field :: atom()
}).

-spec record_occ09
  (#union_field3{}) -> atom().
record_occ09(#union_field3{_ = U, field = A})
  when is_binary(U) -> A;
record_occ09(#union_field3{_ = U, field = _}) ->
  U.

-spec record_occ10
    (#union_field{}) -> binary().
record_occ10(#union_field{_ = U})
  when is_atom(U) -> atom_to_binary(U);
record_occ10(#union_field{_ = U}) ->
  U.

-record(union_field4, {
  x :: integer() | ok,
  y :: integer() | err
}).

-spec record_occ11
    (#union_field4{}) -> integer().
record_occ11(#union_field4{x = A, y = A}) -> A;
record_occ11(#union_field4{}) -> 0.

-spec record_occ12
    (#union_field4{}) -> integer().
record_occ12(#union_field4{_ = A}) -> A;
record_occ12(#union_field4{}) -> 0.

-spec record_occ13_neg
    (#union_field4{}) -> integer().
record_occ13_neg(#union_field4{x = A, y = _}) -> A.

-record(triple_union, {
    field :: #a{} | #b{} | #c{}
}).

-spec triple_union_occ
    (#triple_union{}) -> atom().
triple_union_occ(#triple_union{field = R})
    when
        not is_record(R, a),
        not is_record(R, b)
    -> R#c.id;
triple_union_occ(#triple_union{field = R})
    when
        not is_record(R, a)
    -> R#b.id;
triple_union_occ(#triple_union{field = R})
    -> R#a.id.

% Do we want to support this?
-spec triple_union_occ2
    (#triple_union{}) -> atom().
triple_union_occ2(#triple_union{field = R})
    when
        not is_record(R, a),
        not is_record(R, b)
    -> R#c.id;
triple_union_occ2({triple_union, B})
    when
        not is_record(B, a)
    -> B#b.id;
triple_union_occ2({triple_union, A})
    -> A#a.id.

-record(refrec, {
    z = undefined :: eqwalizer:refinable(undefined | string())
}).

-type refrec1() :: #refrec{z :: string()}.

-spec get_s(refrec1()) -> string().
get_s(#refrec{z = S}) -> S.

-spec occ18
  (a | b) -> b.
occ18(A) when A == a -> b;
occ18(B) -> B.

-spec occ19
  (a | b) -> b.
occ19(A) when A =:= a -> b;
occ19(B) -> B.

-spec occ20
  (a | b) -> b.
occ20(B) when B =/= a -> B;
occ20(_) -> b.

-spec occ21
  (a | b) -> b.
occ21(A) when A =/= b -> b;
occ21(B) -> B.

-spec occ22
({a, A} | {b, B}, fun((B) -> A)) -> A.
occ22({a, A}, _) -> A;
occ22({b, B}, F) -> F(B).

-spec occ23_neg
  ({a, A} | {b, B}, fun((B) -> A)) -> A.
occ23_neg({a, A}, _) -> A;
occ23_neg({_, B}, F) -> F(B).

-spec occ24
(fun(() -> atom()) | atom()) ->
atom().
occ24(F) when is_function(F) -> F();
occ24(A) -> A.

-spec occ25
  (fun(() -> atom()) | atom()) ->
  atom().
occ25(A) when is_atom(A) -> A;
occ25(F) -> F().

-spec occ26_neg
  (fun() | {term()}) ->
  {term()}.
occ26_neg(F) when is_function(F, 1) -> {F(1)};
occ26_neg(T) -> T.

-spec occ27_neg
  (fun() | {term()}) ->
  {term()}.
occ27_neg(T) when is_tuple(T) -> T;
occ27_neg(F) -> F().

-spec occ28_neg
(tuple() | atom()) -> atom().
occ28_neg(R) when is_record(R, a) -> R#a.id;
occ28_neg(A) -> A.

-record(one_field, {
  f1 :: integer()
}).
-record(two_fields1, {
  f1 :: integer(),
  f2 :: integer()
}).
-record(two_fields2, {
  f1 :: integer(),
  f2 :: integer()
}).

% we don't "unfold" records
% as tuples yet
-spec todo04
(#one_field{} | #two_fields1{}) ->
integer().
todo04(X) ->
  case X of
    {_N, _N1} -> X#one_field.f1
  end.

-spec occ29(#one_field{} | #two_fields1{}) -> none().
occ29(X) ->
  case X of
    {} -> X
  end.

-spec occ30({}) -> none().
occ30(X) ->
  if
    is_record(X, one_field) -> X
  end.

-spec occ31
({a, integer()} | {integer(), b}) -> none().
occ31(X) ->
  case X of
    {{}, {}} -> X
  end.

-spec occ32
  ({a | integer(), b | integer()}) -> none().
occ32(X) ->
  case X of
    {{}, {}} -> X
  end.

-spec occ33
(true | false | 'maybe') ->
  boolean() | undefined.
occ33(X) when
  not is_boolean(X) -> undefined;
occ33(B) -> B.

-record(response, {
  id1 :: undefined | number(),
  id2 :: undefined | number(),
  id3 :: undefined | number(),
  id4 :: undefined | number(),
  id5 :: undefined | number(),
  id6 :: undefined | number(),
  id7 :: undefined | number()
}).

-spec extract_id1
    (#response{}) -> number().
extract_id1(#response{
  id1 = Id1,
  id2 = Id2
}) ->
  if
    Id1 =/= undefined -> Id1;
    Id2 =/= undefined -> Id2;
    true -> erlang:error(no_id)
  end.

-spec extract_id2
    (#response{}) -> number().
extract_id2(#response{
  id1 = Id1,
  id2 = Id2,
  id3 = Id3,
  id4 = Id4,
  id5 = Id5,
  id6 = Id6,
  id7 = Id7
}) ->
  if
    Id1 =/= undefined -> Id1;
    Id2 =/= undefined -> Id2;
    Id3 =/= undefined -> Id3;
    Id4 =/= undefined -> Id4;
    Id5 =/= undefined -> Id5;
    Id6 =/= undefined -> Id6;
    Id7 =/= undefined -> Id7;
    true -> erlang:error(no_id)
  end.

-type result(A) ::
  {ok, A} | {err, atom()}.
-spec map_result
    (fun((A) -> B), result(A))
      -> result(B).
map_result(F, Res) ->
  case Res of
    {ok, A} -> {ok, F(A)};
    Err -> Err
  end.

-type choice(A) :: {bad, A} | {good, A}.
-type error() :: {err, atom()}.
-spec get(choice(A) | error())
      -> A | error().
get({Tag, A}) when (Tag == bad);
                   (Tag == good) ->
  A;
get(Err) ->
  Err.

-type rich_tuple() ::
{atom(), integer()} | {integer(), atom()}.
-spec get_int(rich_tuple()) -> integer().
get_int({I, _}) when is_integer(I) -> I;
get_int({_, I}) -> I.

-spec occ_or
    (atom() | integer() | binary())
      -> binary().
occ_or(Arg) ->
  case Arg of
    A when is_atom(A) or
      is_integer(A) -> <<>>;
    B -> B
  end.

-spec occ_orelse
    (atom() | integer() | binary())
      -> binary().
occ_orelse(Arg) ->
  case Arg of
    A when is_atom(A) orelse
      is_integer(A) -> <<>>;
    B -> B
  end.

-spec occ_and
({result(number()), result(number())})
  -> number().
occ_and({{Tag1, I1}, {Tag2, I2}})
  when (Tag1 == ok) and (Tag2 == ok) ->
  I1 + I2;
occ_and(_) ->
  0.

-spec occ_andalso
    ({result(number()), result(number())})
      -> number().
occ_andalso({{Tag1, I1}, {Tag2, I2}})
  when (Tag1 == ok) andalso (Tag2 == ok) ->
  I1 + I2;
occ_andalso(_) ->
  0.

-spec multi_param1(
    term(),
    number() | atom()
) -> number().
multi_param1(_, A) when is_atom(A) -> 1;
multi_param1(_, Y) -> Y.

-spec multi_param2_neg(
    term(),
    number() | atom()
) -> number().
multi_param2_neg(X, Y) when is_atom(X),
                is_atom(Y) -> 1;
multi_param2_neg(_, Y) -> Y.

-spec multi_param3(
    atom(),
    number() | atom()
) -> number().
multi_param3(X, Y) when is_atom(X),
  is_atom(Y) -> 1;
multi_param3(_, Y) -> Y.

-spec multi_param4(
    atom(),
    number() | atom()
) -> {term(), number()}.
multi_param4(X, Y) when is_atom(Y) ->
  {X, 1};
multi_param4(_, Y) ->
  {ok, Y}.

-spec foo1({
  atom() | binary(),
  atom() | binary()
}) ->
  binary().
foo1({X, Y}) when
  is_atom(X) or is_atom(Y) ->
    <<>>;
foo1({B1, B2}) ->
    <<B1/binary, B2/binary>>.

-spec foo2(
  atom() | binary(),
  atom() | binary()
) ->
  binary().
foo2(X, Y) when
  is_atom(X) or is_atom(Y) ->
  <<>>;
foo2(B1, B2) ->
  <<B1/binary, B2/binary>>.

-spec foo3({
  atom() | binary(),
  atom() | binary()
}) ->
  binary().
foo3({B1, B2}) when
  (not is_atom(B1)) and
    (not is_atom(B2)) ->
  <<B1/binary, B2/binary>>;
foo3({_, _}) ->
  <<>>.

-spec foo4(
  atom() | binary(),
  atom() | binary()
) ->
  binary().
foo4(B1, B2) when
  (not is_atom(B1)) and
    (not is_atom(B2)) ->
  <<B1/binary, B2/binary>>;
foo4(_, _) ->
  <<>>.

-spec foo5({
  atom() | binary(),
  atom() | binary()
}) ->
  binary().
foo5({A1, A2}) when
  is_atom(A1) and is_atom(A2) ->
  <<>>;
foo5({A, B}) when
  is_atom(A) ->
  B;
foo5({B, A}) when
  is_atom(A) ->
  B;
foo5({B1, B2}) ->
  <<B1/binary, B2/binary>>.

% compare it with foo5
% occurrence typing for
% multi-param functions
% is not the same as for tuples
-spec foo6(
  atom() | binary(),
  atom() | binary()
) ->
  binary().
foo6(A1, A2) when
  is_atom(A1) and is_atom(A2) ->
  <<>>;
foo6(A, B) when
  is_atom(A) ->
  B;
foo6(B, A) when
  is_atom(A) ->
  B;
foo6(B1, B2) ->
  <<B1/binary, B2/binary>>.

-spec occ34
    (a | b) -> b.
occ34(B) when B /= a -> B;
occ34(_) -> b.

-spec occ35
    (a | b) -> b.
occ35(A) when A /= b -> b;
occ35(B) -> B.

-spec occ36(
    {atom()} | {{atom()}, {atom()}}
) -> {{atom()}, {atom()}}.
occ36({_, _} = P2) -> P2;
occ36(P1) -> {P1, P1}.

-spec occ37(
    {atom()} | {{atom()}, {atom()}}
) -> {{atom()}, {atom()}}.
occ37(P2 = {_, _}) -> P2;
occ37(P1) -> {P1, P1}.

-type t10() :: {
  atom(),
  a | b,
  undefined | binary()
}.
-spec occ38(t10()) -> binary() | no_binary.
occ38({_, a, undefined}) -> <<>>;
occ38({_, a, B}) -> B.

-record(rec10, {
  f1 :: a | b,
  f2 :: undefined | binary()
}).
-spec occ39(#rec10{}) -> binary().
occ39(#rec10{f2 = undefined}) -> <<>>;
occ39(#rec10{f2 = B}) -> B.

-spec occ40(#rec10{}) -> binary().
occ40(#rec10{f1 = a, f2 = undefined}) -> <<>>;
occ40(#rec10{f1 = a, f2 = B}) -> B.

-spec occ41(
    {string() | undefined, string()}
) -> string().
occ41(T = {undefined, S}) -> S;
occ41(T = {S, _}) -> S.

% We do not support "deep" matches
-spec occ42_neg(
    {string() | undefined, string()}
) -> string().

occ42_neg({F, S} = {undefined, _}) -> S;
occ42_neg({F, S} = {_, _}) -> F.

-spec occ43_neg(
    {string() | undefined, string()}
) -> string().

occ43_neg({F, S} = {_, _}) -> S;
occ43_neg({F, S} = {_, _}) -> F.

-record(string_or_def, {
    value :: string() | undefined,
    default :: string()
}).

-spec occ44(#string_or_def{}) -> string().
occ44(R = #string_or_def{value = undefined, default = D}) ->
    D;
occ44(R = #string_or_def{}) ->
    R#string_or_def.value.

-spec occ45(binary() | atom(), atom())
      -> {atom(), atom()}.
occ45(B, A) when is_binary(B) ->
  {undefined, A};
occ45(A1, A2) ->
  {A1, A2}.

-spec occ46({binary() | atom(), atom()}) -> {atom(), atom()}.
occ46(Tuple) ->
  case Tuple of
    {B, A} when is_binary(B) ->
      {undefined, A};
    {A1, A2} ->
      {A1, A2}
  end.

-spec occ47(binary() | atom(), atom())
      -> {atom(), atom()}.
occ47(A1, A2) ->
  case {A1, A2} of
    {B, _} when is_binary(B) ->
      {undefined, A2};
    {A3, A4} ->
      {A3, A4}
  end.

-spec occ48(binary() | atom(), atom())
      -> {atom(), atom()}.
occ48(A1, A2) ->
  case {A1, A2} of
    {B, _} when is_binary(B) ->
      {undefined, A2};
    _ ->
      {A1, A2}
  end.

-spec occ49(
    integer() | undefined,
    integer() | undefined
) -> {integer(), integer()}.
occ49(A1, A2) ->
  case {A1, A2} of
    {undefined, undefined} ->
      {0, 0};
    {undefined, _} ->
      {0, A2};
    {_, undefined} ->
      {A1, 0};
    _ ->
      {A1, A2}
  end.

-spec occ50
    (ok, integer() | undefined)
    -> integer().
occ50(_, undefined) -> 0;
occ50(_, I) -> I.

-spec occ51
    (ok, integer() | undefined)
    -> integer().
occ51(ok, undefined) -> 0;
occ51(ok, I) -> I.

-spec occ52
    ([term()], integer() | undefined)
    -> integer().
occ52(L, undefined) when is_list(L) -> 0;
occ52(_, I) -> I.

-spec occ53_neg
    ([term()], integer() | undefined)
    -> integer().
occ53_neg(L, undefined) when is_integer(L) -> 0;
occ53_neg(_, I) -> I.

-type m() :: #{
    a => a,
    b => b
}.

-spec occ54
    (m(), integer() | undefined)
    -> integer().
occ54(#{}, undefined) -> 0;
occ54(#{}, I) -> I.

-spec occ55
    ([] | atom(), atom()) -> atom().
occ55([], A) -> A;
occ55(A, _) -> A.

-spec occ56
    ([atom()] | atom(), atom()) -> [atom()].
occ56(V, A) ->
    case V of
        [_] -> V;
        _ -> [A]
    end.

-spec occ57_neg
    ([atom()] | atom(), atom()) -> [atom()].
occ57_neg(V, _) ->
    case V of
        [_] -> V;
        A -> [A]
    end.

-spec occ58_neg
    ([atom()] | atom(), atom()) -> [atom()].
occ58_neg(V, A) ->
    case V of
        [_] -> V;
        [] -> V;
        A2 -> [A2]
    end.

-spec occ_binary_pat_1(
  binary() | string(),
  integer()
) -> binary().
occ_binary_pat_1(X, Size) ->
  case X of
    <<_:Size/binary>> ->
      X;
    _ ->
      <<>>
  end.

-spec occ_binary_pat_2_neg(
    binary() | string(),
    integer()
) -> binary().
occ_binary_pat_2_neg(X, Size) ->
  case X of
    <<_:Size/binary>> ->
      X;
    _ ->
      X
  end.

-spec occ_binary_pat_3(
  {binary(), binary()} |
    {atom(), atom()},
  integer()
) -> binary().
occ_binary_pat_3(X, Size) ->
  case X of
    {<<_:Size/binary>>, Bin} ->
      Bin;
    _ ->
      <<>>
  end.

-spec occ_binary_pat_4_neg(
  {binary(), binary()} |
    {atom(), atom()},
  integer()
) -> binary().
occ_binary_pat_4_neg(X, Size) ->
  case X of
    {<<_:Size/binary>>, Bin} ->
      Bin;
    {_, Y} ->
      Y
  end.

-spec occ_guard_binary_1(any()) -> binary().
occ_guard_binary_1(V) when V =:= <<"ok">> -> V;
occ_guard_binary_1(V) -> <<"err">>.

-spec occ_guard_binary_2_neg(any()) -> binary().
occ_guard_binary_2_neg(V) when V =:= <<"ok">>; V =:= ok -> V;
occ_guard_binary_2_neg(V) -> <<"err">>.

-spec occ_guard_binary_3_neg(binary() | ok) -> ok.
occ_guard_binary_3_neg(V) when V =:= <<"ok">> -> ok;
occ_guard_binary_3_neg(V) -> V.

-spec occ_guard_binary_4(any()) -> binary().
occ_guard_binary_4(V) when V =/= <<"ok">> -> <<"err">>;
occ_guard_binary_4(V) -> V.

-spec occ_guard_integer(any()) -> integer().
occ_guard_integer(V) when V =:= 0 -> V;
occ_guard_integer(V) when V =/= 1 -> -1;
occ_guard_integer(V) -> V.

-spec foo7([term()] | number()) -> number().
foo7(Args) ->
    case Args of
        Args when is_list(Args) -> length(Args);
        _ -> Args
    end.

-type my_list() :: {term(), my_list()} | number().

-spec foo8(my_list()) -> number().
foo8(Args) ->
    case Args of
        {_, L} -> 1 + foo8(L);
        _ -> Args
    end.

-spec foo8_neg(my_list()) -> number().
foo8_neg(Args) ->
    case Args of
        {_, Args} -> 1 + foo8_neg(Args);
        _ -> Args
    end.

-spec any_tuple_neg(tuple()) -> ok.
any_tuple_neg(T) ->
    case T of
        _ when is_tuple(T) -> T
    end.

-spec non_linear_1
    ({atom(), a | b}) -> b.
non_linear_1({A, A}) -> b;
non_linear_1({_, a}) -> b;
non_linear_1({_, B}) -> B.

-spec non_linear_2
    (map(), integer() | undefined)
    -> integer().
non_linear_2(#{a := I}, I) -> 0;
non_linear_2(#{}, undefined) -> 0;
non_linear_2(#{}, I) -> I.

-spec non_linear_3
    ({atom(), a | b}) -> b.
non_linear_3({_, a}) -> b;
non_linear_3({A, A}) -> b;
non_linear_3({_, B}) -> B.

-spec non_linear_neg_1
    ({atom(), a | b}) -> b.
non_linear_neg_1({A, A}) -> b;
non_linear_neg_1({_, a}) -> b;
non_linear_neg_1({B, B}) -> B.

-spec andalso_throw_1
    (a | b) -> b.
andalso_throw_1(V) ->
    V =:= a andalso throw(error),
    V.

-spec andalso_throw_1_neg
    (a | b) -> b.
andalso_throw_1_neg(V) ->
    V =:= b andalso throw(error),
    V.

-spec andalso_throw_2
    ({a, ok} | {b, error}) -> ok.
andalso_throw_2(V) ->
    V =:= {b, error} andalso throw(error),
    {_, Res} = V,
    Res.

-spec andalso_throw_3
    (binary() | err) -> binary().
andalso_throw_3(V) ->
    is_atom(V) andalso throw(error),
    V.

-spec negate_atoms_neg
  (a | b | c) -> a | b.
negate_atoms_neg(A) when not ((A == a) orelse (A == b)) -> A;
negate_atoms_neg(A) -> A.

-spec negate_number_neg
  (number()) -> ok.
negate_number_neg(N) when not (N == 1) -> N;
negate_number_neg(_) -> ok.

-spec negate_number
  (number() | atom()) -> number().
negate_number(N) when not (N == 1) -> 0;
negate_number(N) -> N.

-spec refine_dynamic
  (dynamic(), ok | err) -> ok.
refine_dynamic(D, A) when is_atom(D) andalso A == err -> ok;
refine_dynamic(D, A) when is_atom(D) -> A.

-spec negate_fun(F :: fun((number(), number()) -> term()) | ok) -> ok.
negate_fun(F) when is_function(F, 2) -> ok;
negate_fun(F) -> F.

-spec negate_fun_neg(F :: fun((number(), number()) -> term()) | ok) -> ok.
negate_fun_neg(F) when is_function(F, 2) -> F;
negate_fun_neg(F) -> F.
