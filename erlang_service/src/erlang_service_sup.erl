%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
%%==============================================================================
%% Top Level Supervisor
%%==============================================================================
-module(erlang_service_sup).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(supervisor).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([start_link/0]).

%% Supervisor Callbacks
-export([init/1]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, noargs).

%%==============================================================================
%% supervisors callbacks
%%==============================================================================
-spec init(noargs) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(noargs) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 5,
        period => 60
    },
    {ok, Socket} = application:get_env(erlang_service, socket),
    ChildSpecs = [
        #{
            id => erlang_service_server,
            start => {erlang_service_server, start_link, [Socket]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
