%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service).
-compile(warn_missing_spec_all).

-export([main/1]).

-record(state, {io = erlang:group_leader() :: pid()}).
-type state() :: #state{}.
-type id() :: integer().
-type doc_origin() :: edoc | eep48.

-spec main([]) -> no_return().
main(_Args) ->
    configure_logging(),
    erlang:system_flag(backtrace_depth, 20),
    {ok, _} = application:ensure_all_started(erlang_service, permanent),
    State = #state{},
    io:setopts(State#state.io, [binary, {encoding, latin1}]),
    try loop(State)
    catch
        K:R:S ->
            io:format(standard_error, "Erlang service crashing: ~ts~n", [erl_error:format_exception(K, R, S)]),
            erlang:raise(K, R, S)
    end.

-spec loop(state()) -> no_return().
loop(State) ->
    case file:read(State#state.io, 4) of
        {ok, <<Size:32/big>>} ->
            {ok, Data} = file:read(State#state.io, Size),
            erlang_service_server:process(Data),
            loop(State);
        eof ->
            erlang:halt(0);
        Err ->
            io:format(standard_error, "Main loop error ~p~n", [Err]),
            erlang:halt(1)
    end.

-spec configure_logging() -> ok.
configure_logging() ->
    %% The default logger uses standard_io for logging.
    %% This causes the communication between the Erlang Service and the rest
    %% of ELP to break in presence of EDoc errors, so we need to tweak the
    %% logging configuration. Unfortunately, the "type" is a write-once-field and
    %% cannot be updated on the fly, so we have to go through the burdain of removing the
    %% handler, tweak it and install it back.
    HandlerId = default,
    {ok, Handler} = logger:get_handler_config(HandlerId),
    OldConfig = maps:get(config, Handler),
    is_map(OldConfig) orelse error({invalid_logger_config, OldConfig}),
    NewConfig = maps:update(type, standard_error, OldConfig),
    logger:remove_handler(HandlerId),
    ok = logger:add_handler(HandlerId, logger_std_h, maps:update(config, NewConfig, Handler)).
