%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format

%%==============================================================================
%% Derived from
%% https://github.com/erlang-ls/erlang_ls/blob/main/apps/els_core/src/els_io_string.erl
%%==============================================================================
%% This module wraps an fd from a file opened in ram mode in a pid().
%% This is needed by elp_epp.
%%==============================================================================
-module(elp_io_string).

-export([new/1]).

-export([
    start_link/1,
    init/1,
    loop/1,
    skip/3
]).

-type state() :: #{
    buffer := string(),
    original := string(),
    mode := binary | list,
    encoding := latin1 | unicode

}.

-include_lib("kernel/include/file.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new(string() | binary()) -> pid().
new(Str) when is_binary(Str) ->
    new(unicode:characters_to_list(Str));
new(Str) ->
    start_link(Str).

%%------------------------------------------------------------------------------
%% IO server
%%
%% Implementation of a subset of the io protocol in order to only support
%% reading operations.
%%------------------------------------------------------------------------------

-spec start_link(string()) -> pid().
start_link(Str) ->
    spawn(?MODULE, init, [Str]).

-spec init(string()) -> ok.
init(Str) ->
    State = #{buffer => Str, original => Str, mode => list, encoding => latin1},
    ?MODULE:loop(State).

-spec loop(state()) -> ok.
loop(State) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            {Reply, NewSt} = request(Request, State),
            reply(From, ReplyAs, Reply),
            ?MODULE:loop(NewSt);
        {file_request, From, Ref, close} ->
            file_reply(From, Ref, ok);
        {file_request, From, Ref, {position, Pos}} ->
            {Reply, NewState} = file_position(Pos, State),
            file_reply(From, Ref, Reply),
            ?MODULE:loop(NewState);
        {file_request, From, Ref, {read_handle_info, Opts}} ->
            {Reply, NewState} = file_info(Opts, State),
            file_reply(From, Ref, Reply),
            ?MODULE:loop(NewState);
        _Unknown ->
            logger:warning("elp_io_string:_Unkown: [~p]", [_Unknown]),
            ?MODULE:loop(State)
    end.

-spec reply(pid(), pid(), any()) -> any().
reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

-spec file_reply(pid(), pid(), any()) -> ok.
file_reply(From, ReplyAs, Reply) ->
    From ! {file_reply, ReplyAs, Reply},
    ok.

-spec file_info([any()], state()) -> {{ok, any()}, state()}.
file_info(_Opts, #{original := Original} = State) ->
    Sz = length(Original),
    {{ok, #file_info{size = Sz}}, State}.

-spec file_position(integer()|cur, state()) -> {{ok, number() | none()}, state()}.
file_position(cur, #{original := Original, buffer := Buffer} = State) ->
    Pos = length(Original) - length(Buffer),
    {{ok, Pos}, State};
file_position(Pos, #{original := Original} = State) ->
    Buffer = lists:nthtail(Pos, Original),
    {{ok, Pos}, State#{buffer => Buffer}}.

-spec request(any(), state()) -> {string() | ok | eof | {error, request}, state()}.
request({get_chars, _Encoding, _Prompt, N}, #{buffer := Str, mode := Mode} = State) ->
    {ReplyStr, NewStr} = get_chars(N, Str),
    case {ReplyStr, Mode} of
      {eof, _} -> Reply = ReplyStr;
      {_, list} -> Reply = ReplyStr;
      {_, binary} -> Reply = unicode:characters_to_binary(ReplyStr)
    end,
    {Reply, State#{buffer => NewStr}};
request({get_line, _Encoding, _Prompt}, #{buffer := Str} = State) ->
    {Reply, NewStr} = get_line(Str),
    {Reply, State#{buffer => NewStr}};
request({get_until, _Encoding, _Prompt, Module, Function, XArgs},  #{buffer := Str} = State) ->
    {Reply, NewStr} = get_until(Module, Function, XArgs, Str),
    {Reply, State#{buffer => NewStr}};
request(getopts, #{mode := Mode, encoding := Encoding} = State) ->
    case Mode of
      binary -> {[{binary, true}, {encoding, Encoding}], State};
      list ->   {[{binary, false}, {encoding, Encoding}], State}
    end;
request({setopts, Opts}, #{encoding := Encoding0} = State) ->
    case lists:keyfind(binary, 1, Opts) of
       {binary, true} -> Binary0 = true;
       _ -> Binary0 = false
    end,
    Binary1 = lists:member(binary, Opts),
    case Binary0 or Binary1 of
      true -> Mode = binary;
      false -> Mode = list
    end,
    case lists:keyfind(binary, 1, Opts) of
        {encoding, latin1} -> Encoding1 = latin1;
        {encoding, unicode} -> Encoding1 = unicode;
        %% Following is as per https://www.erlang.org/doc/apps/stdlib/io.html#setopts/2
        {encoding, utf8} -> Encoding1 = unicode;
        _ -> Encoding1 = Encoding0
    end,
    {ok, State#{mode => Mode, encoding => Encoding1}};
request(_Other, State) ->
    logger:warning("elp_io_string:request:_Other: [~p]", [_Other]),
    {{error, request}, State}.

-spec get_chars(integer(), string()) -> {string() | eof, string()}.
get_chars(_N, []) ->
    {eof, []};
get_chars(1, [Ch | Str]) ->
    {[Ch], Str};
get_chars(N, Str) ->
    do_get_chars(N, Str, []).

-spec do_get_chars(integer(), string(), string()) -> {string(), string()}.
do_get_chars(0, Str, Result) ->
    {lists:flatten(Result), Str};
do_get_chars(_N, [], Result) ->
    {Result, []};
do_get_chars(N, [Ch | NewStr], Result) ->
    do_get_chars(N - 1, NewStr, [Result, Ch]).

-spec get_line(string()) -> {string() | eof, string()}.
get_line([]) ->
    {eof, []};
get_line(Str) ->
    do_get_line(Str, []).

-spec do_get_line(string(), string()) -> {string() | eof, string()}.
do_get_line([], Result) ->
    {lists:flatten(Result), []};
do_get_line("\r\n" ++ RestStr, Result) ->
    {lists:flatten(Result), RestStr};
do_get_line("\n" ++ RestStr, Result) ->
    {lists:flatten(Result), RestStr};
do_get_line("\r" ++ RestStr, Result) ->
    {lists:flatten(Result), RestStr};
do_get_line([Ch | RestStr], Result) ->
    do_get_line(RestStr, [Result, Ch]).

-spec get_until(module(), atom(), list(), term()) ->
    {term(), string()}.
get_until(Module, Function, XArgs, Str) ->
    apply_get_until(Module, Function, [], Str, XArgs).

-spec apply_get_until(module(), atom(), any(), string() | eof, list()) ->
    {term(), string()}.
apply_get_until(Module, Function, State, String, XArgs) ->
    case apply(Module, Function, [State, String | XArgs]) of
        {done, Result, NewStr} ->
            {Result, NewStr};
        {more, NewState} ->
            apply_get_until(Module, Function, NewState, eof, XArgs)
    end.

-spec skip(string() | {cont, integer(), string()}, term(), integer()) ->
    {more, {cont, integer(), string()}} | {done, integer(), string()}.
skip(Str, _Data, Length) when is_list(Str) ->
    {more, {cont, Length, Str}};
skip({cont, 0, Str}, _Data, Length) ->
    {done, Length, Str};
skip({cont, Length, []}, _Data, Length) ->
    {done, eof, []};
skip({cont, Length, [_ | RestStr]}, _Data, _Length) ->
    {more, {cont, Length - 1, RestStr}}.
