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

-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%% API
-export([ get_docs/2
]).

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
-type state() :: #{io := erlang:group_leader()}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

get_docs(Data, DocOrigin) ->
    gen_server:cast(?SERVER, {get_docs, Data, DocOrigin}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(noargs) -> {ok, state()}.
init(noargs) ->
    State = #{io => erlang:group_leader()},
    {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({get_docs, Data, DocOrigin}, #{io := IO} = State) ->
    Pid = spawn_link(fun() ->
        {Id, FileName} = binary_to_term(Data),
        try
            Result = erlang_service:run_get_docs(Id, FileName, DocOrigin),
            erlang_service:reply(Id, Result, IO)
        catch
            Class:Reason:StackTrace ->
                Formatted = erl_error:format_exception(Class, Reason, StackTrace),
                ExceptionData = unicode:characters_to_binary(Formatted),
                erlang_service:reply_exception(Id, ExceptionData, IO)
        end
            end),
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Request, State) ->
    {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
