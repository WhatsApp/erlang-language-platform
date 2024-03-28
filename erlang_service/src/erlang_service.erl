%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service).

-export([main/1]).

-record(state, {io = erlang:group_leader() :: pid()}).
-type state() :: #state{}.
-type id() :: binary().
-type size() :: binary().
-type doc_origin() :: edoc | eep48.

-spec main([]) -> no_return().
main(_Args) ->
    configure_logging(),
    erlang:system_flag(backtrace_depth, 20),
    {ok, _} = application:ensure_all_started(erlang_service, permanent),
    State = #state{},
    io:setopts(State#state.io, [binary, {encoding, latin1}]),
    loop(State).

-spec loop(state()) -> no_return().
loop(State0) ->
    case io:get_line(State0#state.io, "") of
        Line when is_binary(Line) ->
            State = process(binary_part(Line, 0, byte_size(Line) - 1), State0),
            loop(State);
        _ ->
            erlang:halt(1)
    end.

-spec process(binary(), state()) -> state().
process(<<"ADD_PATHS ", BinLen/binary>>, State) ->
    add_paths(BinLen, State);
process(<<"COMPILE ", Binary/binary>>, State) ->
    [Id, BinLen] = binary:split(Binary, <<" ">>, [global]),
    PostProcess = fun(Forms, _FileName) -> term_to_binary({ok, Forms, []}) end,
    % ETF files are consumed by eqwalizer,
    % which requires full paths for snapshot tests.
    elp_lint(Id, BinLen, State, PostProcess, false);
process(<<"TEXT ", Binary/binary>>, State) ->
    [Id, BinLen] = binary:split(Binary, <<" ">>, [global]),
    PostProcess =
        fun(Forms, _) ->
            unicode:characters_to_binary([io_lib:format("~p.~n", [Form]) || Form <- Forms])
        end,
    elp_lint(Id, BinLen, State, PostProcess, false);
process(<<"DOC_EDOC ", Binary/binary>>, State) ->
    [Id, BinLen] = binary:split(Binary, <<" ">>, [global]),
    get_docs(Id, BinLen, State, edoc);
process(<<"DOC_EEP48 ", Binary/binary>>, State) ->
    [Id, BinLen] = binary:split(Binary, <<" ">>, [global]),
    get_docs(Id, BinLen, State, eep48);
process(<<"CT_INFO ", Binary/binary>>, State) ->
    [Id, BinLen] = binary:split(Binary, <<" ">>, [global]),
    ct_info(Id, BinLen, State);
process(<<"EXIT">>, State) ->
    init:stop(),
    State.

-spec add_paths(size(), state()) -> state().
add_paths(BinLen, State) ->
    Len = binary_to_integer(BinLen),
    Paths = collect_paths(Len, State),
    code:add_pathsa(Paths),
    State.

-spec collect_paths(non_neg_integer(), state()) -> [string()].
collect_paths(0, _State) ->
    [];
collect_paths(Len, State) ->
    case io:get_line(State#state.io, "") of
        eof ->
            [];
        Line ->
            Path =
                unicode:characters_to_list(
                    string:trim(Line, trailing)
                ),
            is_list(Path) orelse error({invalid_line, Line}),
            [Path | collect_paths(Len - 1, State)]
    end.

-spec get_docs(id(), size(), state(), doc_origin()) -> state().
get_docs(Id, BinLen, State, DocOrigin) ->
    Data = read_request(BinLen, State#state.io),
    erlang_service_server:get_docs(Id, Data, DocOrigin),
    State.

-spec ct_info(id(), size(), state()) -> state().
ct_info(Id, BinLen, State) ->
    Data = read_request(BinLen, State#state.io),
    erlang_service_server:ct_info(Id, Data),
    State.

-spec elp_lint(id(), size(), state(), fun((any(), any()) -> binary()), boolean()) -> state().
elp_lint(Id, BinLen, State, PostProcess, Deterministic) ->
    Data = read_request(BinLen, State#state.io),
    erlang_service_server:elp_lint(Id, Data, PostProcess, Deterministic),
    State.

-spec read_request(size(), pid()) -> string() | binary().
read_request(BinLen, Device) ->
    Len = binary_to_integer(BinLen),
    %% Use file:read/2 since it reads bytes
    {ok, Data} = file:read(Device, Len),
    Data.

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
