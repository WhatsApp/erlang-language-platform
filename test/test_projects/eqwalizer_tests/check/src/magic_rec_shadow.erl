%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(magic_rec_shadow).

-compile([export_all, nowarn_export_all]).

%% The record defined here shadows the record `magic_rec`
%% of the same arity from the module `magic_rec`.
-record(magic_rec, {id :: integer()}).

-spec local_wins(term()) -> {magic_rec, integer()}.
local_wins(R)
    when is_record(R, magic_rec, 2) -> R;
local_wins(_) -> #magic_rec{id = 0}.

-spec local_wins_neg(term()) -> {magic_rec, atom()}.
local_wins_neg(R)
    when is_record(R, magic_rec, 2) -> R;
local_wins_neg(_) -> {magic_rec, undefined}.
