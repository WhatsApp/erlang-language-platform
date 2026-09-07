%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(magic_rec).

-compile([export_all, nowarn_export_all]).

-export_type([t/0]).

%% A private record defined - without a header file - in the
%% module of the same name: other modules can test it only
%% via `is_record(X, magic_rec, 2)`.
-record(magic_rec, {id :: atom()}).

-type t() :: #magic_rec{}.

-spec new(atom()) -> t().
new(Id) -> #magic_rec{id = Id}.

-spec id(t()) -> atom().
id(#magic_rec{id = Id}) -> Id.
