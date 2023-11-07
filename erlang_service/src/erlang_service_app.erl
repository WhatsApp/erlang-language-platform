%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%==============================================================================
%% Application Callback Module
%%==============================================================================
-module(erlang_service_app).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(application).

%%==============================================================================
%% Exports
%%==============================================================================
%% Application Callbacks
-export([
    start/2,
    stop/1
]).

%%==============================================================================
%% Application Callbacks
%%==============================================================================
-spec start(normal, noargs) -> {ok, pid()}.
start(_StartType, noargs) ->
    erlang_service_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
