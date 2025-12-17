%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(bad_maps).

-compile([export_all, nowarn_export_all]).

-type bad_map_1() :: #{atom() := integer()}.

-type bad_map_2() :: #{a => b, atom() => integer()}.

-type bad_map_3() :: #{a => b, atom() := integer()}.

-spec bad_map_spec(#{a => b, atom() => integer()}) -> ok.
    bad_map_spec(M) -> ok.
