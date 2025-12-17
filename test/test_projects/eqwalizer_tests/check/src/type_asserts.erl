%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(type_asserts).

-compile([export_all, nowarn_export_all]).

-include_lib("eqwalizer/include/eqwalizer.hrl").

-spec assert1(term()) -> binary().
assert1(Arg) ->
  is_binary(Arg) orelse throw(bad_arg),
  Arg.

-spec assert2(term()) -> binary().
assert2(Arg) ->
  is_binary(Arg) orelse error(bad_arg),
  Arg.

-spec assert3(binary() | undefined)
      -> binary().
assert3(Arg) ->
  Arg =/= undefined orelse throw(bad_arg),
  Arg.

-spec assert4(binary() | undefined)
      -> binary().
assert4(Arg) ->
  Arg =/= undefined orelse error(bad_arg),
  Arg.

-spec assert5_neg(
    string() | binary()
) -> binary().
assert5_neg(Arg) ->
  is_list(Arg) orelse throw(bad_arg),
  Arg.

%% we don't support not yet
-spec assert6(
    string() | binary()
) -> binary().
assert6(Arg) ->
  (not is_list(Arg))
    orelse throw(bad_arg),
  Arg.

-spec any_to_atom1(
    string() | binary()
) -> false | atom().
any_to_atom1(A) ->
  Res = is_list(A)
    andalso list_to_atom(A),
  Res.

-spec any_to_atom2(
    string() | binary()
) -> false | atom().
any_to_atom2(A) ->
  is_binary(A)
    andalso list_to_atom(A).

-spec any_to_atom3_neg(
    term()
) -> false | atom().
any_to_atom3_neg(A) ->
  is_binary(A)
    andalso list_to_atom(A).

-spec double_andalso(
    term(), term()
) -> false | {number(), atom()}.
double_andalso(N, A) ->
  is_number(N)
    andalso is_atom(A)
      andalso {N, A}.

-spec double_andalso_neg(
    term(), term()
) -> false |  {atom(), number()}.
double_andalso_neg(N, A) ->
  is_number(N)
    andalso is_atom(A)
    andalso {N, A}.

-spec scope_neg(term())
      -> {false | number(), number()}.
scope_neg(A) ->
  X = is_number(A) andalso A,
  {X, A}.

-spec assert7(
    string() | binary()
) -> binary().
assert7(Input) ->
  true = is_binary(Input),
  Input.

-spec checked_cast1(atom()) -> ok.
checked_cast1(A) -> ?checked_cast(A, dynamic()).

-spec checked_cast1_neg(atom()) -> ok.
checked_cast1_neg(A) -> ?checked_cast(A, ok).

-spec checked_cast2_neg(ok) -> ok.
checked_cast2_neg(A) -> ?checked_cast(A, atom()).

-type invalid() :: foo:non_exist().

-spec checked_cast3_neg(atom()) -> ok.
checked_cast3_neg(A) -> ?checked_cast(A, foo:non_exist()).

-spec checked_cast4_neg(atom()) -> ok.
checked_cast4_neg(A) -> ?checked_cast(A, {ok, invalid()}).

-type nested_map() :: #{dynamic() => #{ka => va, kb => vb, kc => vc}}.

-spec checked_cast_foldl_neg(dynamic()) -> nested_map().
checked_cast_foldl_neg(K) ->
  lists:foldl(fun (Key, Acc) ->
    case Key of
      a ->
        V = maps:get(K, Acc, #{}),
        Acc#{K => V#{ka => va}};
      b ->
        V = maps:get(K, Acc, #{}),
        Acc#{K => V#{kb => vb}};
      c ->
        V = maps:get(K, Acc, #{}),
        Acc#{K => V#{kc => vc}}
    end
  end, #{}, [a, b, c]).

-spec checked_cast_foldl(dynamic()) -> nested_map().
checked_cast_foldl(K) ->
  lists:foldl(fun (Key, Acc) ->
    case Key of
      a ->
        V = maps:get(K, Acc, #{}),
        Acc#{K => V#{ka => va}};
      b ->
        V = maps:get(K, Acc, #{}),
        Acc#{K => V#{kb => vb}};
      c ->
        V = maps:get(K, Acc, #{}),
        Acc#{K => V#{kc => vc}}
    end
  end, ?checked_cast(#{}, nested_map()), [a, b, c]).

-spec unsafe_cast1(atom()) -> ok.
unsafe_cast1(A) -> ?unchecked_cast(A, ok).

-spec unsafe_cast1_neg(atom()) -> ok.
unsafe_cast1_neg(A) -> ?unchecked_cast(A, err).

-spec unsafe_cast2_neg(dynamic()) -> ok.
unsafe_cast2_neg(D) -> ?unchecked_cast(a + D, ok).

-spec vars_in_cast1_neg(A) -> A.
vars_in_cast1_neg(T) when is_atom(T) ->
  ?checked_cast(ok, A).

-spec vars_in_cast2_neg(A | B) -> A | B.
vars_in_cast2_neg(T) when is_atom(T) ->
  ?unchecked_cast(ok, A | B).

-record(rec, {field :: atom()}).

-spec bad_record_field() -> #rec{}.
bad_record_field() ->
  ?unchecked_cast(1, #rec{bad_field :: atom()}).
