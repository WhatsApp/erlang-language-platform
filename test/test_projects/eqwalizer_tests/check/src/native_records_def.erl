%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(native_records_def). % @OTP29Only

-if(?OTP_RELEASE >= 29).

-export([mk_priv/0]).

-export_record([pub]).

-record #pub{a :: integer(), b = 0 :: integer()}.
-record #priv{a :: integer()}.

-spec mk_priv() -> #priv{}.
mk_priv() -> #priv{a = 1}.

-endif.
