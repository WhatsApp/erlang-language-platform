%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

%%% Records with invalid field types are kept
%%% (with the invalid fields made dynamic),
%%% but referring to them is an error.

-module(invalid_records).

-compile([export_all, nowarn_export_all]).
-export_type([
    alias_of_bad/0, refines_bad/0,
    cyclic_next/0
]).

-include("invalid_records.hrl").

%% Only the invalid field is made dynamic,
%% the other fields keep their types.
-record(bad, {
    ok_field :: atom(),
    bad_field :: unknown:t()
}).

-callback cb_uses_bad(#bad{}) -> ok.

-spec use_bad() -> ok.
use_bad() ->
    R = #bad{
        ok_field = a,
        bad_field = 42
    },
    eqwalizer:reveal_type(
        R#bad.ok_field
    ),
    eqwalizer:reveal_type(
        R#bad.bad_field
    ),
    ok.

-spec use_bad_neg() -> ok.
use_bad_neg() ->
    _ = #bad{ok_field = 42},
    ok.

-spec spec_uses_bad(#bad{}) -> ok.
spec_uses_bad(_) -> ok.

-spec overloaded_uses_bad
    (#bad{}) -> ok;
    (atom()) -> ok.
overloaded_uses_bad(_) -> ok.

-type alias_of_bad() :: #bad{}.

-spec uses_alias_of_bad
    (alias_of_bad()) -> ok.
uses_alias_of_bad(_) -> ok.

-type refines_bad() ::
    #bad{ok_field :: 'a'}.

%% Refers to an invalid record: reported, and
%% *all* the fields are made dynamic.
-record(wraps_bad, {
    inner :: #bad{},
    other :: atom()
}).

-spec use_wraps_bad() -> ok.
use_wraps_bad() ->
    R = #wraps_bad{other = 42},
    eqwalizer:reveal_type(
        R#wraps_bad.other
    ),
    ok.

%% Invalid on its own *and* referring to an
%% invalid record: both are reported, and all
%% the fields are made dynamic.
-record(bad_and_wraps_bad, {
    bad_field :: unknown:t(),
    inner :: #bad{},
    other :: atom()
}).

-spec use_bad_and_wraps_bad() -> ok.
use_bad_and_wraps_bad() ->
    R = #bad_and_wraps_bad{other = 42},
    eqwalizer:reveal_type(
        R#bad_and_wraps_bad.other
    ),
    ok.

%% Invalid on its own and self-referencing:
%% it is not its own transitive cause.
-record(self_ref, {
    bad_field :: unknown:t(),
    next :: #self_ref{} | undefined
}).

-spec use_self_ref() -> ok.
use_self_ref() ->
    R = #self_ref{next = undefined},
    eqwalizer:reveal_type(
        R#self_ref.next
    ),
    ok.

%% Same, through a cycle with an alias: only
%% the alias is reported.
-record(cyclic, {
    bad_field :: unknown:t(),
    next :: cyclic_next()
}).
-type cyclic_next() ::
    #cyclic{} | undefined.

%% Declared in a header: the invalid field is
%% not reported here, uses of it still are.
-spec spec_uses_hdr_bad
    (#hdr_bad{}) -> ok.
spec_uses_hdr_bad(_) -> ok.

-spec use_hdr_bad() -> ok.
use_hdr_bad() ->
    R = #hdr_bad{ok_field = a},
    eqwalizer:reveal_type(
        R#hdr_bad.ok_field
    ),
    eqwalizer:reveal_type(
        R#hdr_bad.bad_field
    ),
    ok.
