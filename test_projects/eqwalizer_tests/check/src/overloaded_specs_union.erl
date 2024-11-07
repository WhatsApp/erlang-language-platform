%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.
-module(overloaded_specs_union).

-compile([export_all, nowarn_export_all]).

-spec over
    (atom()) -> binary();
    (binary()) -> atom().
over(A) when is_atom(A) -> atom_to_binary(A);
over(B) when is_binary(B) -> binary_to_atom(B).

-spec use_over_neg_1(atom() | pid()) -> binary().
use_over_neg_1(Arg) -> over(Arg).

-spec use_over_neg_2(atom() | binary() | pid()) -> binary().
use_over_neg_2(Arg) -> over(Arg).
