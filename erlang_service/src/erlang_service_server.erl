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

-export([path_open/4]).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%% API
-export_type([id/0, file_id/0, file_text/0]).

%%==============================================================================
%% Includes
%%==============================================================================

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #{io := port(), requests := [request_entry()], own_requests := [own_request_entry()]}.
-type request_entry() :: {pid(), id(), reference() | infinity}.
-type own_request_entry() :: {id(), gen_server:from()}.
-type result() :: {result, id(), [segment()]}.
-type exception() :: {exception, id(), any()}.
-type id() :: integer().
-type file_id() :: integer().
-type file_text() :: string().
-type include_type() :: normal | lib | doc.
-type segment() :: {string(), binary()}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

-define(RECURSIVE_CALLBACK_TIMEOUT, infinity).

-spec path_open(id(), string(), file_id(), include_type())
   -> {ok, pid(), string(), file_id()} | {error, any()}.
path_open(ReqId, Name, FileId, IncludeType) ->
  case gen_server:call(?SERVER, {request, ReqId,
             [unicode:characters_to_binary(add_include_type(Name, FileId, IncludeType))]},
              ?RECURSIVE_CALLBACK_TIMEOUT) of
    {ok, <<SzPath:32, Paths:SzPath/binary,
           NewFileId:32,
           Sz:32, FileText:Sz/binary>>} ->
        Pid = elp_io_string:new(FileText),
        {ok, Pid, binary_to_list(Paths), NewFileId};
     X -> X
  end.

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(noargs) -> {ok, state()}.
init(noargs) ->
    %% Open stdin/out as a port, requires node to be started with -noinput
    %% We do this to avoid the overhead of the normal Erlang stdout/in stack
    %% which is very significant for raw binary data, mostly because it's prepared
    %% to work with unicode input and does multiple encoding/decoding rounds for raw bytes
    Port = open_port({fd, 0, 1}, [
        eof,
        binary,
        {packet, 4},
        %% Disable busy limits.
        %% Experience shows that if they are enabled they can cause
        %% the entire process to hang, on the send to the device, when
        %%  the machine is under load.
        {busy_limits_port, disabled},
        {busy_limits_msgq, disabled}
    ]),
    State = #{io => Port, requests => [], own_requests => []},
    {ok, State}.

-spec handle_call({request, id(), any()}, gen_server:from(), state())
   -> {noreply, state()} | {stop|reply, any(), state()}.
handle_call({request, ReqId, Data}, From,
            #{io := IO, requests := Requests, own_requests := OwnRequests} = State) ->
    case lists:keytake(ReqId, 2, Requests) of
        {value, {_Pid, Id, _}, _NewRequests} ->
            request(Id, Data, IO),
            {noreply, State#{own_requests => [{Id, From}|OwnRequests]}};
        _ ->
            {reply, failed, State}
    end;
handle_call(Req, _From, State) ->
    {stop, {unexpected_request, Req}, State}.

-spec handle_cast(result() | exception(), state()) -> {noreply, state()}.
handle_cast({result, Id, Result}, #{io := IO, requests := Requests} = State) ->
    case lists:keytake(Id, 2, Requests) of
        {value, {_Pid, _Id, infinity}, NewRequests} ->
            reply(Id, Result, IO),
            {noreply, State#{requests => NewRequests}};
        {value, {_Pid, _Id, Timer}, NewRequests} ->
            erlang:cancel_timer(Timer),
            reply(Id, Result, IO),
            {noreply, State#{requests => NewRequests}};
        _ ->
            {noreply, State}
    end;
handle_cast({exception, Id, ExceptionData}, #{io := IO, requests := Requests} = State) ->
    case lists:keytake(Id, 2, Requests) of
        {value, {_Pid, _Id, infinity}, NewRequests} ->
            reply_exception(Id, ExceptionData, IO),
            {noreply, State#{requests => NewRequests}};
        {value, {_Pid, _Id, Timer}, NewRequests} ->
            erlang:cancel_timer(Timer),
            reply_exception(Id, ExceptionData, IO),
            {noreply, State#{requests => NewRequests}};
        _ ->
            {noreply, State}
    end.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({IO, {data, Data}}, #{io := IO} = State) when is_binary(Data) ->
    handle_request(Data, State);
handle_info({IO, eof}, #{io := IO} = State) ->
    %% stdin closed, we're done
    reply(-1, <<"EXT">>, IO),
    erlang:halt(0),
    {noreply, State};
handle_info({timeout, Pid}, #{io := IO, requests := Requests} = State) ->
    case lists:keytake(Pid, 1, Requests) of
        {value, {Pid, Id, _Timer}, NewRequests} ->
            exit(Pid, normal),
            reply_exception(Id, <<"Timeout">>, IO),
            {noreply, State#{requests => NewRequests}};
        _ ->
            {noreply, State}
    end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
reply(Id, Data, Device) ->
    Reply = [<<Id:64/big, 0>> | Data],
    Device ! {self(), {command, Reply}},
    ok.

reply_exception(Id, Data, Device) ->
    Reply = [<<Id:64/big, 1>> | Data],
    Device ! {self(), {command, Reply}},
    ok.

request(Id, Data, Device) ->
    Req = [<<Id:64/big, 2>> | Data],
    Device ! {self(), {command, Req}},
    ok.

-spec process_request_async(atom(), id(), binary(), [any()]) -> pid().
process_request_async(Module, Id, Data, AdditionalParams) ->
    spawn_link(
        fun() ->
            try
                Params = binary_to_term(Data),
                case Module:run(Id, Params ++ AdditionalParams) of
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

-spec handle_request(binary(), state()) -> {noreply, state()}.
handle_request(<<"ACP", _:64/big, Data/binary>>, State) ->
    Paths = collect_paths(Data),
    code:add_pathsa(Paths),
    {noreply, State};
handle_request(<<"COM", Id:64/big, Sz:32, FileText:Sz/binary, Data/binary>>, State) ->
    PostProcess = fun(Forms, _FileName) -> term_to_binary({ok, Forms, []}) end,
    request(erlang_service_lint, Id, Data, [FileText, PostProcess, false], infinity, State);
handle_request(<<"TXT", Id:64/big, Sz:32, FileText:Sz/binary, Data/binary>>, State) ->
    PostProcess =
        fun(Forms, _) ->
            unicode:characters_to_binary([io_lib:format("~p.~n", [Form]) || Form <- Forms])
        end,
    request(erlang_service_lint, Id, Data, [FileText, PostProcess, false], infinity, State);
handle_request(<<"DCE", Id:64/big, Sz:32, AST:Sz/binary, Data/binary>>, State) ->
    request(erlang_service_edoc, Id, Data, [edoc, AST], infinity, State);
handle_request(<<"DCP", Id:64/big, Data/binary>>, State) ->
    request(erlang_service_edoc, Id, Data, [eep48, no_ast], infinity, State);
handle_request(<<"CTI", Id:64/big, Sz:32, AstBinary:Sz/binary, Data/binary>>, State) ->
    request(erlang_service_ct, Id, Data, [AstBinary], 10_000, State);
%% Start of callback responses
handle_request(<<"REP", OrigId:64/big, Status:8, Data/binary>>,
               #{own_requests := OwnRequests} = State) ->
    case lists:keytake(OrigId, 1, OwnRequests) of
        {value, {OrigId, ReplyFrom}, NewOwnRequests} ->
            case Status of
                0 -> gen_server:reply(ReplyFrom, {ok, Data});
                _ -> gen_server:reply(ReplyFrom, {error, Data})
            end,
            {noreply, State#{own_requests => NewOwnRequests}};
        _ ->
            {noreply, State}
    end.


-spec request(module(), id(), binary(), [any()], timeout(), state()) -> {noreply, state()}.
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

-spec collect_paths(binary()) -> [file:filename()].
collect_paths(<<>>) -> [];
collect_paths(<<Size:32/big, Data:Size/binary, Rest/binary>>) ->
    [binary_to_list(Data) | collect_paths(Rest)].

-spec encode_segments([segment()]) -> iodata().
encode_segments(Segments) ->
    %% collapse to iovec for efficiently sending between processes
    erlang:iolist_to_iovec([encode_segment(Segment) || Segment <- Segments]).

-spec encode_segment(segment()) -> [iodata()].
encode_segment({Tag, Data}) when byte_size(Tag) =:= 3 ->
    [Tag, <<(byte_size(Data)):32/big>> | Data].

-spec add_include_type(string(), file_id(), include_type()) -> io_lib:chars().
add_include_type(Path, FileId, IncludeType) ->
    case IncludeType of
        normal -> io_lib:format("~p:N:~s", [FileId, Path]);
        lib    -> io_lib:format("~p:L:~s", [FileId, Path]);
        doc    -> io_lib:format("~p:D:~s", [FileId, Path])
    end.
