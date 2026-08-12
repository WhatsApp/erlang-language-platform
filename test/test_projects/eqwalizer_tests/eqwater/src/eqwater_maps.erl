%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater_maps).

-compile([export_all, nowarn_export_all]).

-spec map_occ_01(#{a := integer()} | ok) -> ok.
map_occ_01(#{a := I}) when is_integer(I) -> ok;
map_occ_01(V) -> V.

-spec map_occ_02(#{a := term(), b := integer()})
    -> #{a := integer(), b := integer()}.
map_occ_02(M = #{a := I}) when is_integer(I) -> M;
map_occ_02(M) -> M#{a => 0}.

-spec map_occ_03(#{a => term(), b => integer()})
    -> #{a := integer(), b => integer()}.
map_occ_03(M = #{a := I}) when is_integer(I) -> M;
map_occ_03(M) -> M#{a => 0}.

-spec map_occ_04_neg(#{a => term(), b => integer()})
    -> #{a := integer(), b := integer()}.
map_occ_04_neg(M = #{a := I}) when is_integer(I) -> M;
map_occ_04_neg(M) -> M#{a => 0}.

-spec map_occ_05(#{a => integer(), b => integer()})
    -> #{a := integer(), b := integer()}.
map_occ_05(M = #{a := _, b := _}) -> M;
map_occ_05(M) -> M#{a => 0, b => 0}.

-spec map_occ_06_neg(#{a => integer()} | ok) -> ok.
map_occ_06_neg(#{a := I}) when is_integer(I) -> ok;
map_occ_06_neg(V) -> V.

-spec map_occ_07_neg
    (#{a := integer()} | #{c := integer()})
    -> #{b := integer()}.
map_occ_07_neg(#{a := _}) -> #{b => 0};
map_occ_07_neg(M) -> M.

-type foo() :: #{} | #{required := binary(), optional => binary()}.

-spec add_optional(foo(), binary()) -> foo().
add_optional(Foo = #{required := _R1}, Optional) ->
  Foo#{optional => Optional};
add_optional(Z, _) ->
  Z.

-spec map_occ_08_neg
  (#{eqwalizer:dynamic() => eqwalizer:dynamic()} | #{c => integer()})
  -> err.
map_occ_08_neg(#{a := _} = M) -> M;
map_occ_08_neg(_) -> err.

-spec map_occ_09(#{a => undefined | map()} | #{b => term()}) -> term().
map_occ_09(#{a := undefined}) -> 1;
map_occ_09(#{a := Map}) -> Map#{2 => 2};
map_occ_09(_) -> 3.

-spec is_ok(ok) -> ok.
is_ok(ok) -> ok.

-spec map_occ_foreach_neg(#{term() => #{a => ok | err}}) -> ok.
map_occ_foreach_neg(M) ->
  maps:foreach(fun
    (_, #{a := err}) -> ok;
    (_, #{a := V}) -> is_ok(V)
  end, M).

%% Values selected by a key that is not a compile-time literal.
%% Such a key addresses no position in the map type, but the value
%% pattern still binds variables, and guards still refine them.

-record(frag, {index :: undefined | integer()}).
-record(job, {part :: undefined | binary()}).
-type replica() :: full | witness.

-spec map_occ_var_key_01(atom(), #{atom() => undefined | integer()})
    -> integer().
map_occ_var_key_01(K, M) ->
  case M of
    #{K := V} when V =/= undefined -> V;
    _ -> 0
  end.

-spec map_occ_var_key_02(atom(), #{atom() => integer() | binary()})
    -> integer().
map_occ_var_key_02(K, M) ->
  case M of
    #{K := V} when is_integer(V) -> V;
    _ -> 0
  end.

-spec map_occ_var_key_03(atom(), #{atom() => #frag{}}) -> integer().
map_occ_var_key_03(K, Frags) ->
  case Frags of
    #{K := #frag{index = Idx}} when Idx =/= undefined -> Idx;
    _ -> 0
  end.

-spec map_occ_var_key_04(atom(), #{atom() => #frag{}})
    -> {integer(), #frag{}}.
map_occ_var_key_04(K, Frags) ->
  case Frags of
    #{K := #frag{index = Idx} = Frag} when Idx =/= undefined -> {Idx, Frag};
    _ -> {0, #frag{index = 0}}
  end.

-spec map_occ_var_key_05
    (atom(), #{atom() => false | indeterminate | replica()})
    -> replica().
map_occ_var_key_05(K, M) ->
  case M of
    #{K := T} when T =/= false, T =/= indeterminate -> T;
    _ -> full
  end.

-spec map_occ_var_key_06
    (#{atom() => undefined | integer()}, #{atom() => term()})
    -> [integer()].
map_occ_var_key_06(M, Keys) ->
  maps:fold(
    fun(K, _, Acc) ->
      case M of
        #{K := V} when V =/= undefined -> [V | Acc];
        _ -> Acc
      end
    end,
    [],
    Keys
  ).

-spec map_occ_var_key_07(atom())
    -> fun((#{atom() => undefined | integer()}) -> integer()).
map_occ_var_key_07(K) ->
  fun
    (#{K := V}) when V =/= undefined -> V;
    (_) -> 0
  end.

-spec map_occ_var_key_08(atom(), #{atom() => #frag{}})
    -> #{{atom(), integer()} => #frag{}}.
map_occ_var_key_08(K, Frags) ->
  case Frags of
    #{K := #frag{index = Idx} = Frag} when Idx =/= undefined ->
      #{{K, Idx} => Frag};
    _ -> #{}
  end.

-spec map_occ_var_key_09
    (atom(), #{atom() => undefined | {atom(), integer()}})
    -> atom().
map_occ_var_key_09(K, M) ->
  case M of
    #{K := P} when P =/= undefined -> element(1, P);
    _ -> undefined
  end.

-spec map_occ_var_key_10(atom(), #{atom() => #job{}}) -> binary().
map_occ_var_key_10(K, Jobs) ->
  case Jobs of
    #{K := #job{part = undefined}} -> <<>>;
    #{K := #job{part = P}} when P =/= undefined -> P;
    _ -> <<>>
  end.
