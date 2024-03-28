%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service).

-export([main/1]).

-spec main([]) -> ok.
main([Socket]) ->
    configure_logging(),
    erlang:system_flag(backtrace_depth, 20),
    application:set_env(erlang_service, socket, Socket, [{persistent, true}]),
    {ok, _} = application:ensure_all_started(erlang_service, permanent),
    timer:sleep(infinity).

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
