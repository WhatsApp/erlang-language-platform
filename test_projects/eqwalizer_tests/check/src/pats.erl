%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(pats).

-compile([export_all, nowarn_export_all]).

-spec return_map() -> #{a => ok} | error.
return_map() -> #{a => ok}.

-spec patmatch1() -> ok.
patmatch1() ->
    #{a := V} = _M = return_map(),
    V.

-spec patmatch2() -> ok.
patmatch2() ->
    #{a := _V} = M = return_map(),
    maps:get(a, M).

-spec patmatch3_neg() -> ok.
patmatch3_neg() ->
    _V = M = return_map(),
    maps:get(a, M).

-spec patmatch4() -> ok.
patmatch4() ->
    M = #{a := _V} = return_map(),
    maps:get(a, M).
