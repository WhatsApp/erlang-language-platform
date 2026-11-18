%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(native_records_neg). % @OTP29Only

-compile([export_all, nowarn_export_all]).

-if(?OTP_RELEASE >= 29).

-spec unbound() -> term().
unbound() -> #native_records_def:missing{a = 1}.

-spec undefined_field() -> term().
undefined_field() -> #native_records_def:pub{a = 1, c = 2}.

-spec missing_required() -> term().
missing_required() -> #native_records_def:pub{b = 1}.

-spec non_exported() -> term().
non_exported() -> #native_records_def:priv{a = 1}.

-spec undefined_anon_field(#native_records_def:pub{}) -> term().
undefined_anon_field(#_{zzz = Z}) -> Z.

-endif.
