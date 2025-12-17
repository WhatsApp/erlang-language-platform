%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(lists_tests).

-compile([export_all, nowarn_export_all]).

-spec lists_union_neg
    (atom(), binary()) -> [atom()] | [binary()].
lists_union_neg(V1, V2) -> [V1, V2].

-spec lists_union_2_neg
    (atom(), [atom()] | [binary()]) -> [atom()] | [binary()].
lists_union_2_neg(V, L) -> [V | L].

-type union1() :: a | b.
-type union2() :: union1() | c.

-spec unit_list1(union2()) -> [a] | [b] | [c].
unit_list1(X) -> [X].

-spec unit_list2(union2()) -> [union2()].
unit_list2(X) -> [X].

-spec unit_list3(union2()) -> [union1()] | [c].
unit_list3(X) -> [X].
