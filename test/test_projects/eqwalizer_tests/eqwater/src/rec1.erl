%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

% used in eqwater_records.erl
-module(rec1).

-export_type([rec/0]).

-record(rec, {f1 :: atom()}).
-type rec() :: #rec{}.
