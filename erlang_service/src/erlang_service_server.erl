%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
%%==============================================================================
%% The server responsible for co-ordinating work
%%==============================================================================
-module(erlang_service_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #{socket := gen_tcp:socket(), requests := [request_entry()]}.
-type request_entry() :: {pid(), id(), reference() | infinity}.
-type result() :: {result, id(), [segment()]}.
-type exception() :: {exception, id(), any()}.
-type id() :: integer().
-type segment() :: {string(), binary()}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(string()) -> gen_server:start_ret().
start_link(Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Socket, []).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(string()) -> {ok, state()}.
init(Socket) ->
    {ok, Sock} = gen_tcp:connect({local, Socket}, 0, [binary, local, {packet, 4}, {active, true}]),
    {ok, #{socket => Sock, requests => []}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(result() | exception(), state()) -> {noreply, state()}.
handle_cast({result, Id, Result}, #{socket := Socket, requests := Requests} = State) ->
    case lists:keytake(Id, 2, Requests) of
        {value, {_Pid, _Id, infinity}, NewRequests} ->
            reply(Id, Result, Socket),
            {noreply, State#{requests => NewRequests}};
        {value, {_Pid, _Id, Timer}, NewRequests} ->
            erlang:cancel_timer(Timer),
            reply(Id, Result, Socket),
            {noreply, State#{requests => NewRequests}};
        _ ->
            {noreply, State}
    end;
handle_cast({exception, Id, ExceptionData}, #{socket := Socket, requests := Requests} = State) ->
    case lists:keytake(Id, 2, Requests) of
        {value, {_Pid, _Id, infinity}, NewRequests} ->
            reply_exception(Id, ExceptionData, Socket),
            {noreply, State#{requests => NewRequests}};
        {value, {_Pid, _Id, Timer}, NewRequests} ->
            erlang:cancel_timer(Timer),
            reply_exception(Id, ExceptionData, Socket),
            {noreply, State#{requests => NewRequests}};
        _ ->
            {noreply, State}
    end.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({tcp, Socket, Data}, #{socket := Socket} = State) ->
    handle_request(Data, State);
handle_info({tcp_closed, Socket}, #{socket := Socket} = State) ->
    init:stop(),
    {noreply, State};
handle_info({timeout, Pid}, #{socket := Socket, requests := Requests} = State) ->
    case lists:keytake(Pid, 1, Requests) of
        {value, {Pid, Id, _Timer}, NewRequests} ->
            exit(Pid, normal),
            reply_exception(Id, <<"Timeout">>, Socket),
            {noreply, State#{requests => NewRequests}};
        _ ->
            {noreply, State}
    end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
reply(Id, Data, Socket) ->
    ok = gen_tcp:send(Socket, [<<Id:64/big, 0>> | Data]),
    ok.

reply_exception(Id, Data, Socket) ->
    ok = gen_tcp:send(Socket, [<<Id:64/big, 1>> | Data]),
    ok.

handle_request(<<"ACP", _:64/big, Data/binary>>, State) ->
    Paths = collect_paths(Data),
    code:add_pathsa(Paths),
    {noreply, State};
handle_request(<<"COM", Id:64/big, Data/binary>>, State) ->
    PostProcess = fun(Forms, _FileName) -> term_to_binary({ok, Forms, []}) end,
    request(erlang_service_lint, Id, Data, [PostProcess, false], infinity, State);
handle_request(<<"TXT", Id:64/big, Data/binary>>, State) ->
    PostProcess =
        fun(Forms, _) ->
            unicode:characters_to_binary([io_lib:format("~p.~n", [Form]) || Form <- Forms])
        end,
    request(erlang_service_lint, Id, Data, [PostProcess, false], infinity, State);
handle_request(<<"DCE", Id:64/big, Data/binary>>, State) ->
    request(erlang_service_edoc, Id, Data, [edoc], infinity, State);
handle_request(<<"DCP", Id:64/big, Data/binary>>, State) ->
    request(erlang_service_edoc, Id, Data, [eep48], infinity, State);
handle_request(<<"CTI", Id:64/big, Data/binary>>, State) ->
    request(erlang_service_ct, Id, Data, [], 10_000, State).

request(Module, Id, Data, AdditionalParams, Timeout, #{requests := Requests} = State) ->
    Pid = process_request_async(Module, Id, Data, AdditionalParams),
    Timer =
        case Timeout of
            infinity ->
                infinity;
            Timeout ->
                erlang:send_after(Timeout, ?SERVER, {timeout, Pid})
        end,
    {noreply, State#{requests => [{Pid, Id, Timer} | Requests]}}.

collect_paths(<<>>) -> [];
collect_paths(<<Size:32/big, Data:Size/binary, Rest/binary>>) ->
    [Data | collect_paths(Rest)].

-spec process_request_async(atom(), id(), binary(), [any()]) -> pid().
process_request_async(Module, Id, Data, AdditionalParams) ->
    spawn_link(
        fun() ->
            try
                Params = binary_to_term(Data),
                case Module:run(Params ++ AdditionalParams) of
                    {ok, Result} ->
                        gen_server:cast(?SERVER, {result, Id, encode_segments(Result)});
                    {error, Error} ->
                        gen_server:cast(?SERVER, {exception, Id, Error})
                end
            catch
                Class:Reason:StackTrace ->
                    Formatted = erl_error:format_exception(Class, Reason, StackTrace),
                    ExceptionData = unicode:characters_to_binary(Formatted),
                    gen_server:cast(?SERVER, {exception, Id, ExceptionData})
            end
        end
    ).

encode_segments(Segments) ->
    erlang:iolist_to_iovec([encode_segment(Segment) || Segment <- Segments]).

encode_segment({Tag, Data}) when byte_size(Tag) =:= 3 ->
    [<<Tag:3/binary, (byte_size(Data)):32/big>> | Data].
