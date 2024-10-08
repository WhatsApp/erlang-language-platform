%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.
-module(overloaded_specs_dynamic_result).

-compile([export_all, nowarn_export_all]).

-record(rec1, {foo :: atom()}).
-record(rec2, {foo :: atom()}).


-spec overloaded1
    (atom(), #rec1{}) -> #rec1{};
    (atom(), #rec2{}) -> #rec2{}.
overloaded1(Foo, #rec1{}) -> #rec1{foo = Foo};
overloaded1(Foo, #rec2{}) -> #rec2{foo = Foo}.

-spec use_overloaded1(dynamic(), #rec1{}) -> #rec1{}.
use_overloaded1(Foo, Rec) -> overloaded1(Foo, Rec).
