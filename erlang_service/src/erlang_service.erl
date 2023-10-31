-module(erlang_service).

-export([main/1]).

-record(state, {io = erlang:group_leader()}).

main(_Args) ->
    configure_logging(),
    erlang:system_flag(backtrace_depth, 20),
    {ok, _} = application:ensure_all_started(erlang_service, permanent),
    State = #state{},
    io:setopts(State#state.io, [binary, {encoding, latin1}]),
    loop(State).

loop(State0) ->
    case io:get_line(State0#state.io, "") of
        Line when is_binary(Line) ->
            State = process(binary_part(Line, 0, byte_size(Line) - 1), State0),
            loop(State);
        _ ->
            erlang:halt(1)
    end.

process(<<"ADD_PATHS ", BinLen/binary>>, State) ->
    add_paths(BinLen, State);
process(<<"COMPILE ", BinLen/binary>>, State) ->
    PostProcess = fun(Forms, _FileName) -> term_to_binary({ok, Forms, []}) end,
    % ETF files are consumed by eqwalizer,
    % which requires full paths for snapshot tests.
    elp_lint(BinLen, State, PostProcess, false);
process(<<"TEXT ", BinLen/binary>>, State) ->
    PostProcess =
        fun(Forms, _) ->
            unicode:characters_to_binary([io_lib:format("~p.~n", [Form]) || Form <- Forms])
        end,
    elp_lint(BinLen, State, PostProcess, false);
process(<<"DOC_EDOC ", BinLen/binary>>, State) ->
    get_docs(BinLen, State, edoc);
process(<<"DOC_EEP48 ", BinLen/binary>>, State) ->
    get_docs(BinLen, State, eep48);
process(<<"CT_INFO ", BinLen/binary>>, State) ->
    ct_info(BinLen, State);
process(<<"EXIT">>, State) ->
    init:stop(),
    State.

add_paths(BinLen, State) ->
    Len = binary_to_integer(BinLen),
    Paths = collect_paths(Len, State),
    code:add_pathsa(Paths),
    State.

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
            [Path | collect_paths(Len - 1, State)]
    end.

get_docs(BinLen, State, DocOrigin) ->
    Len = binary_to_integer(BinLen),
    %% Use file:read/2 since it reads bytes
    {ok, Data} = file:read(State#state.io, Len),
    erlang_service_server:get_docs(Data, DocOrigin),
    State.

ct_info(BinLen, State) ->
    Len = binary_to_integer(BinLen),
    %% Use file:read/2 since it reads bytes
    {ok, Data} = file:read(State#state.io, Len),
    erlang_service_server:ct_info(Data),
    State.

elp_lint(BinLen, State, PostProcess, Deterministic) ->
    Len = binary_to_integer(BinLen),
    %% Use file:read/2 since it reads bytes
    {ok, Data} = file:read(State#state.io, Len),
    erlang_service_server:elp_lint(Data, PostProcess, Deterministic),
    State.

%-----------------------------------------------------------------------
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
    NewConfig = maps:update(type, standard_error, OldConfig),
    logger:remove_handler(HandlerId),
    logger:add_handler(HandlerId, logger_std_h, maps:update(config, NewConfig, Handler)).
