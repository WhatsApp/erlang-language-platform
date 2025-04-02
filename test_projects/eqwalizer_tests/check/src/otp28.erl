%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(otp28).

-compile([export_all, nowarn_export_all]).

-nominal foo() :: ok | error.

-spec mk_nominal() -> foo().
mk_nominal() -> ok.

-spec mk_nominal_neg() -> foo().
mk_nominal_neg() -> warning.
