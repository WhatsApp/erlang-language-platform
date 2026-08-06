%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(maps_tests).

-compile([export_all, nowarn_export_all]).

-spec put_1_neg(atom()) -> integer().
put_1_neg(K) -> maps:get(a, maps:put(K, <<"boom">>, #{a => 1})).

-spec update_1_neg(atom()) -> integer().
update_1_neg(K) -> M = #{a => 1}, maps:get(a, M#{K => <<"boom">>}).
