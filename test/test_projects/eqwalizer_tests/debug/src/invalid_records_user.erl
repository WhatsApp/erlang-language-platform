%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

%%% Records invalid in another module
%%% invalidate their uses here as well.

-module(invalid_records_user).

-compile([export_all, nowarn_export_all]).

-spec uses_remote_alias
    (invalid_records:alias_of_bad()) -> ok.
uses_remote_alias(_) -> ok.

-spec uses_remote_refined
    (invalid_records:refines_bad()) -> ok.
uses_remote_refined(_) -> ok.

-type local_alias() ::
    invalid_records:alias_of_bad().

-spec uses_local_alias
    (local_alias()) -> ok.
uses_local_alias(_) -> ok.

-record(wraps_remote, {
    inner ::
        invalid_records:alias_of_bad(),
    other :: atom()
}).

-spec use_wraps_remote() -> ok.
use_wraps_remote() ->
    R = #wraps_remote{other = 42},
    eqwalizer:reveal_type(
        R#wraps_remote.other
    ),
    ok.
