%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% % @format
%% Erlang token scanning functions of io library.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037    NUL - US        control
%% 040 - 057    SPC - /         punctuation
%% 060 - 071    0 - 9           digit
%% 072 - 100    : - @           punctuation
%% 101 - 132    A - Z           uppercase
%% 133 - 140    [ - `           punctuation
%% 141 - 172    a - z           lowercase
%% 173 - 176    { - ~           punctuation
%% 177          DEL             control
%% 200 - 237                    control
%% 240 - 277    NBSP - ¿        punctuation
%% 300 - 326    À - Ö           uppercase
%% 327          ×               punctuation
%% 330 - 336    Ø - Þ           uppercase
%% 337 - 366    ß - ö           lowercase
%% 367          ÷               punctuation
%% 370 - 377    ø - ÿ           lowercase
%%
%% Many punctuation characters have special meaning:
%%  $\s, $_, $", $$, $%, $', $.
%% DEL is a punctuation.
%%
%% Must watch using × \327, very close to x \170.

-module(elp_scan).

%%% External exports

-export([
    tokens/3, tokens/4,
    line/1,
    location/1,
    symbol/1,
    format_error/1,
    reserved_word/1
]).

-export_type([
    error_info/0,
    options/0,
    return_cont/0,
    token/0,
    tokens/0,
    tokens_result/0,
    location/0
]).

%%%
%%% Defines and type definitions
%%%

-define(OFFSET(O), (is_integer(O) andalso O >= 0)).
-define(STRING(S), is_list(S)).
-define(RESWORDFUN(F), is_function(F, 1)).

-type offset() :: non_neg_integer().
-type location() :: {offset(), offset()}.
-type category() :: atom().
-type resword_fun() :: fun((atom()) -> boolean()).
-type option() :: {'reserved_word_fun', resword_fun()}.
-type options() :: option() | [option()].
-type symbol() :: atom() | float() | integer() | string().
-type token() ::
    {category(), location(), symbol()}
    | {category(), location()}.
-type tokens() :: [token()].
-type error_description() :: term().
-type error_info() :: {offset(), module(), error_description()}.

%%% Local record.
-record(erl_scan, {
    resword_fun = fun reserved_word/1 :: resword_fun()
}).

%%----------------------------------------------------------------------------

-spec format_error(ErrorDescriptor) -> string() when
    ErrorDescriptor :: error_description().
format_error({unterminated, char}) ->
    "unterminated character";
format_error({unterminated, What, Head}) ->
    %% The reported position should be the first char in Head
    lists:flatten(
        [
            "unterminated ",
            string_thing(What),
            " starting with ",
            io_lib:write_string(Head, string_quote(What))
        ]
    );
format_error({illegal, Type}) ->
    lists:flatten(io_lib:fwrite("illegal ~w", [Type]));
format_error({base, Base}) ->
    lists:flatten(io_lib:fwrite("illegal base '~w'", [Base]));
format_error(indentation) ->
    "bad indentation in triple-quoted string";
format_error(white_space) ->
    "not white space after start of triple-quoted string";
format_error(string_concat) ->
    "adjacent string literals without intervening white space";
format_error(Other) ->
    lists:flatten(io_lib:write(Other)).

-type char_spec() :: string() | 'eof'.
-type cont_fun() :: fun(
    (
        char_spec(),
        #erl_scan{},
        offset(),
        tokens(),
        any()
    ) -> any()
).
-opaque return_cont() ::
    {erl_scan_continuation, string(), offset(), tokens(), #erl_scan{}, any(), cont_fun()}.
-type tokens_result() ::
    {'ok', Tokens :: tokens(), EndOffset :: offset()}
    | {'eof', EndOffset :: offset()}
    | {'error', ErrorInfo :: error_info(), EndOffset :: offset()}.

-spec tokens(Continuation, CharSpec, StartOffset) -> Return when
    Continuation :: return_cont() | [],
    CharSpec :: char_spec(),
    StartOffset :: offset(),
    Return ::
        {'done', Result :: tokens_result(), LeftOverChars :: char_spec()}
        | {'more', Continuation1 :: return_cont()}.
tokens(Cont, CharSpec, StartOffset) ->
    tokens(Cont, CharSpec, StartOffset, []).

-spec tokens(Continuation, CharSpec, StartOffset, Options) -> Return when
    Continuation :: return_cont() | [],
    CharSpec :: char_spec(),
    StartOffset :: offset(),
    Options :: options(),
    Return ::
        {'done', Result :: tokens_result(), LeftOverChars :: char_spec()}
        | {'more', Continuation1 :: return_cont()}.
tokens([], CharSpec, Off, Options) when ?OFFSET(Off) ->
    tokens1(CharSpec, options(Options), Off, [], fun scan/5, []);
tokens({erl_scan_continuation, Cs, Off, Toks, St, Any, Fun}, CharSpec, _Off, _Opts) ->
    tokens1(Cs ++ CharSpec, St, Off, Toks, Fun, Any).

-spec line(Token) -> erl_anno:line() when
    Token :: token().
line(_Token) ->
    0.

-spec location(Token) -> location() when
    Token :: token().

location(Token) ->
    element(2, Token).

-spec symbol(Token) -> symbol() when
    Token :: token().

symbol({Category, _Anno}) ->
    Category;
symbol({_Category, _Anno, Symbol}) ->
    Symbol;
symbol(T) ->
    erlang:error(badarg, [T]).

%%%
%%% Local functions
%%%

string_thing(What) ->
    case What of
        atom ->
            "atom";
        string ->
            "string";
        {string, _N} ->
            "triple-quoted string";
        {sigil, Name, Q1, Q2} ->
            [$~, atom_to_list(Name), Q1, Q2, " sigil string"];
        {sigil, Name, _N} ->
            "triple-quoted " ++ [$~, atom_to_list(Name) | " sigil string"]
    end.

%'
string_quote(atom) -> $';
%"
string_quote(_) -> $".

-define(WHITE_SPACE(C),
    (is_integer(C) andalso
        (C >= $\000 andalso C =< $\s orelse C >= $\200 andalso C =< $\240))
).
-define(DIGIT(C), (is_integer(C) andalso $0 =< C andalso C =< $9)).
-define(CHAR(C), (is_integer(C) andalso 0 =< C andalso C < 16#110000)).
-define(UNICODE(C),
    (is_integer(C) andalso
        (C >= 0 andalso C < 16#D800 orelse
            C > 16#DFFF andalso C < 16#FFFE orelse
            C > 16#FFFF andalso C =< 16#10FFFF))
).

-define(UNI255(C), (is_integer(C) andalso 0 =< C andalso C =< 16#ff)).

-define(NO_SIGIL, {}).

options(Opts0) when is_list(Opts0) ->
    Opts = lists:foldr(fun expand_opt/2, [], Opts0),
    [RW_fun] =
        case opts(Opts, [reserved_word_fun], []) of
            badarg ->
                erlang:error(badarg, [Opts0]);
            R ->
                R
        end,
    #erl_scan{
        resword_fun = RW_fun
    };
options(Opt) ->
    options([Opt]).

opts(Options, [Key | Keys], L) ->
    V =
        case lists:keyfind(Key, 1, Options) of
            {reserved_word_fun, F} when ?RESWORDFUN(F) ->
                {ok, F};
            {Key, _} ->
                badarg;
            false ->
                {ok, default_option(Key)}
        end,
    case V of
        badarg ->
            badarg;
        {ok, Value} ->
            opts(Options, Keys, [Value | L])
    end;
opts(_Options, [], L) ->
    lists:reverse(L).

default_option(reserved_word_fun) ->
    fun reserved_word/1.

expand_opt(return, Os) ->
    [return_comments, return_white_spaces | Os];
expand_opt(O, Os) ->
    [O | Os].

tokens1(Cs, St, Off, Toks, Fun, Any) when ?STRING(Cs); Cs =:= eof ->
    case Fun(Cs, St, Off, Toks, Any) of
        {more, {Cs0, Nst, Noff, Ntoks, Nany, Nfun}} ->
            {more, {erl_scan_continuation, Cs0, Noff, Ntoks, Nst, Nany, Nfun}};
        {ok, Toks0, eof, Noff} ->
            Res =
                case Toks0 of
                    [] ->
                        {eof, Noff};
                    _ ->
                        {ok, lists:reverse(Toks0), Noff}
                end,
            {done, Res, eof};
        {ok, Toks0, Rest, Noff} ->
            {done, {ok, lists:reverse(Toks0), Noff}, Rest};
        {{error, _, _} = Error, Rest} ->
            {done, Error, Rest}
    end.

scan(Cs, #erl_scan{} = St, Off, Toks, _) ->
    scan1(Cs, St, Off, Toks).

scan1([$\s | Cs], St, Off, Toks) ->
    skip_white_space(Cs, St, Off, Toks, 1);
scan1([$\n | Cs], St, Off, Toks) ->
    skip_white_space(Cs, St, Off, Toks, 1);
%% Optimization: some very common punctuation characters:
scan1([$, | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ',', 1);
scan1([$( | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '(', 1);
scan1([$) | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ')', 1);
scan1([${ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '{', 1);
scan1([$} | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '}', 1);
scan1([$[ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '[', 1);
scan1([$] | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ']', 1);
scan1([$; | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ';', 1);
scan1([$_ = C | Cs], St, Off, Toks) ->
    scan_variable(Cs, St, Off, Toks, {[C], 1});
scan1([$\% | Cs], St, Off, Toks) ->
    skip_comment(Cs, St, Off, Toks, 1);
%% More punctuation characters below.
scan1([C | _], _St, _Off, _Toks) when not ?CHAR(C) ->
    error({not_character, C});
scan1([C | Cs], St, Off, Toks) when C >= $A, C =< $Z ->
    scan_variable(Cs, St, Off, Toks, {[C], 1});
scan1([C | Cs], St, Off, Toks) when C >= $a, C =< $z ->
    scan_atom(Cs, St, Off, Toks, {[C], 1});
scan1([C | Cs], St, Off, Toks) when ?DIGIT(C) ->
    scan_number(Cs, St, Off, Toks, [C], no_underscore);
scan1("..." ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '...', 3);
scan1(".." = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
scan1(".." ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '..', 2);
scan1("." = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
scan1("&&" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '&&', 2);
scan1("&" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
scan1("&" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '&', 1);
scan1([$. | Cs], St, Off, Toks) ->
    scan_dot(Cs, St, Off, Toks, 1);
%' Emacs
scan1([$' | Cs], St, Off, Toks) ->
    scan_qatom(Cs, St, Off, Toks);
%" Emacs
scan1([$" | _] = Cs, St, Off, Toks) ->
    scan_string(Cs, St, Off, Toks, ?NO_SIGIL);
scan1([$~ = C | Cs], St, Off, Toks) ->
    scan_sigil_prefix(Cs, St, Off, Toks, {[C], 1});
scan1([$$ | Cs], St, Off, Toks) ->
    scan_char(Cs, St, Off, Toks, 1);
scan1([C | Cs], St, Off, Toks) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_atom(Cs, St, Off, Toks, {[C], 2});
scan1([C | Cs], St, Off, Toks) when C >= $À, C =< $Þ, C /= $× ->
    scan_variable(Cs, St, Off, Toks, {[C], 2});
scan1([$\t | Cs], St, Off, Toks) ->
    skip_white_space(Cs, St, Off, Toks, 1);
scan1([C | Cs], St, Off, Toks) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, St, Off, Toks, char_byte_size(C));
%% Punctuation characters and operators, first recognise multiples.
%% ?= for the maybe ... else ... end construct
scan1("?=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '?=', 2);
scan1("?" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% << <:- <- <:= <=
scan1("<<" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<<', 2);

scan1("<:-" ++Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<:-', 3);
scan1("<-" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<-', 2);
scan1("<:=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<:=', 3);
scan1("<=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<=', 2);
scan1("<:" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
scan1("<" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% >> >=
scan1(">>" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '>>', 2);
scan1(">=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '>=', 2);
scan1(">" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% -> --
scan1("->" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '->', 2);
scan1("--" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '--', 2);
scan1("-" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% ++
scan1("++" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '++', 2);
scan1("+" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% =:= =/= =< == =>
scan1("=:=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=:=', 3);
scan1("=:" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
scan1("=/=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=/=', 3);
scan1("=/" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
scan1("=<" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=<', 2);
scan1("=>" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=>', 2);
scan1("==" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '==', 2);
scan1("=" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% /=
scan1("/=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '/=', 2);
scan1("/" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% ||
scan1("||" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '||', 2);
scan1("|" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% :=
scan1(":=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ':=', 2);
%% :: for typed records
scan1("::" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '::', 2);
scan1(":" = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
%% Optimization: punctuation characters less than 127:
scan1([$= | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=', 1);
scan1([$: | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ':', 1);
scan1([$| | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '|', 1);
scan1([$# | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '#', 1);
scan1([$/ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '/', 1);
scan1([$? | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '?', 1);
scan1([$- | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '-', 1);
scan1([$+ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '+', 1);
scan1([$* | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '*', 1);
scan1([$< | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<', 1);
scan1([$> | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '>', 1);
scan1([$! | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '!', 1);
scan1([$@ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '@', 1);
scan1([$\\ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '\\', 1);
scan1([$^ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '^', 1);
scan1([$` | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '`', 1);
scan1([$~ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '~', 1);
%% End of optimization.
scan1([C | Cs], St, Off, Toks) when ?UNI255(C) ->
    tok2(Cs, St, Off, Toks, list_to_atom([C]), char_byte_size(C));
scan1([C | Cs], _St, Off, _Toks) when ?CHAR(C) ->
    scan_error({illegal, character}, Off, Off + 1, Cs);
scan1([] = Cs, St, Off, Toks) ->
    {more, {Cs, St, Off, Toks, [], fun scan/5}};
scan1(eof = Cs, _St, Off, Toks) ->
    {ok, Toks, Cs, Off}.

scan_atom_fun(Cs, #erl_scan{} = St, Off, Toks, Ncs) ->
    scan_atom(Cs, St, Off, Toks, Ncs).

scan_atom(Cs0, St, Off, Toks, {Ncs0, N0}) ->
    case scan_name(Cs0, Ncs0, N0) of
        {more, Ncs, N} ->
            {more, {[], St, Off, Toks, {Ncs, N}, fun scan_atom_fun/5}};
        {done, WcsR, Cs, N} ->
            Wcs = lists:reverse(WcsR),
            try list_to_atom(Wcs) of
                Name ->
                    case (St#erl_scan.resword_fun)(Name) of
                        true ->
                            tok2(Cs, St, Off, Toks, Name, N);
                        false ->
                            tok3(Cs, St, Off, Toks, atom, Name, N)
                    end
            catch
                _:_ ->
                    scan_error({illegal, atom}, Off, Off + N, Cs)
            end
    end.

scan_variable_fun(Cs, #erl_scan{} = St, Off, Toks, Ncs) ->
    scan_variable(Cs, St, Off, Toks, Ncs).

scan_variable(Cs0, St, Off, Toks, {Ncs0, N0}) ->
    case scan_name(Cs0, Ncs0, N0) of
        {more, Ncs, N} ->
            {more, {[], St, Off, Toks, {Ncs, N}, fun scan_variable_fun/5}};
        {done, WcsR, Cs, N} ->
            Wcs = lists:reverse(WcsR),
            try list_to_atom(Wcs) of
                Name ->
                    tok3(Cs, St, Off, Toks, var, Name, N)
            catch
                _:_ ->
                    scan_error({illegal, var}, Off, Off + N, Cs)
            end
    end.

scan_name([C | _] = Cs, Wcs, N) when not ?CHAR(C) ->
    {done, Wcs, Cs, N};
scan_name([C | Cs], Wcs, N) when C >= $a, C =< $z ->
    scan_name(Cs, [C | Wcs], N + 1);
scan_name([C | Cs], Wcs, N) when C >= $A, C =< $Z ->
    scan_name(Cs, [C | Wcs], N + 1);
scan_name([$_ = C | Cs], Wcs, N) ->
    scan_name(Cs, [C | Wcs], N + 1);
scan_name([C | Cs], Wcs, N) when ?DIGIT(C) ->
    scan_name(Cs, [C | Wcs], N + 1);
scan_name([$@ = C | Cs], Wcs, N) ->
    scan_name(Cs, [C | Wcs], N + 1);
scan_name([C | Cs], Wcs, N) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_name(Cs, [C | Wcs], N + 2);
scan_name([C | Cs], Wcs, N) when C >= $À, C =< $Þ, C =/= $× ->
    scan_name(Cs, [C | Wcs], N + 2);
scan_name([], Wcs, N) ->
    {more, Wcs, N};
scan_name(Cs, Wcs, N) ->
    {done, Wcs, Cs, N}.

scan_dot([$% | _] = Cs, _St, Off, Toks, N) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N};
scan_dot([$\n | Cs], _St, Off, Toks, N) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N + 1};
scan_dot([C | Cs], _St, Off, Toks, N) when ?WHITE_SPACE(C) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N + char_byte_size(C)};
scan_dot(eof = Cs, _St, Off, Toks, N) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N};
scan_dot(Cs, St, Off, Toks, N) ->
    tok2(Cs, St, Off, Toks, '.', N).

skip_white_space_fun(Cs, #erl_scan{} = St, Off, Toks, N) ->
    skip_white_space(Cs, St, Off, Toks, N).

%% TODO: optimise for common chars
skip_white_space([C | Cs], St, Off, Toks, N) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, St, Off, Toks, N + char_byte_size(C));
skip_white_space([] = Cs, St, Off, Toks, N) ->
    {more, {Cs, St, Off, Toks, N, fun skip_white_space_fun/5}};
skip_white_space(Cs, St, Off, Toks, N) ->
    scan1(Cs, St, Off + N, Toks).

scan_char([$\\ | Cs], St, Off, Toks, N0) ->
    case scan_escape(Cs, N0 + 1) of
        more ->
            {more, {[$\\ | Cs], St, Off, Toks, N0, fun scan_char/5}};
        {error, Ncs, Error, N} ->
            scan_error(Error, Off, Off + N, Ncs);
        {eof, N} ->
            scan_error({unterminated, char}, Off, Off + N, eof);
        {Val, _Str, Ncs, N} ->
            %"
            Ntoks = [{char, {Off, Off + N}, Val} | Toks],
            scan1(Ncs, St, Off + N, Ntoks)
    end;
scan_char([$\n = C | Cs], St, Off, Toks, N) ->
    scan1(Cs, St, Off + N + 1, [{char, {Off, Off + N + 1}, C} | Toks]);
scan_char([C | Cs], St, Off, Toks, N) when ?UNICODE(C) ->
    Noff = Off + N + char_byte_size(C),
    scan1(Cs, St, Noff, [{char, {Off, Noff}, C} | Toks]);
scan_char([C | _Cs], _St, Off, _Toks, N) when ?CHAR(C) ->
    scan_error({illegal, character}, Off, Off + N + char_byte_size(C), eof);
scan_char([], St, Off, Toks, N) ->
    {more, {[], St, Off, Toks, N, fun scan_char/5}};
scan_char(eof, _St, Off, _Toks, N) ->
    scan_error({unterminated, char}, Off, Off + N, eof).

%% Sigil Prefix is scanned here and handled in scan_tqstring/6
%% and scan_qstring/8 where non-verbatim sigils (that handle
%% character escape sequences) are enumerated.
%% Search for SigilType or sigil_type.
%%
%% The bogus SigilType = {} is used to indicate a regular
%% non-sigil string to scan_qstring/8 and scan_sigil_suffix/6.
%%
%% Sigils are also handled in erl_parse:build_sigil/3 that
%% enumerates all sigils, cause an error for unknown ones,
%% and transforms known ones.
%%
scan_sigil_prefix(Cs, St, Off, Toks, {Wcs, N0}) ->
    case scan_name(Cs, Wcs, N0) of
        {more, Nwcs, N} ->
            {more, {[], St, Off, Toks, {Nwcs, N}, fun scan_sigil_prefix/5}};
        {done, Nwcs, Ncs, N} ->
            Type = sigil_prefix,
            SigilCs = lists:reverse(Nwcs),
            try list_to_atom(tl(SigilCs)) of
                SigilType when is_atom(SigilType) ->
                    Tok = {Type, {Off, Off + N}, SigilType},
                    scan_string(Ncs, St, Off + N, [Tok | Toks], SigilType)
            catch
                _:_ ->
                    scan_error({illegal, Type}, Off, Off + N, Ncs)
            end
    end.

%% Assuming that a string starts with a delimiter right here
scan_string(Cs, St, Off, Toks, SigilType) ->
    case Cs of
        %"
        [$", $", $" | Ncs] ->
            scan_tqstring(Ncs, St, Off, Toks, SigilType, 3);
        [$", $"] ->
            {more, {Cs, St, Off, Toks, SigilType, fun scan_string/5}};
        [$"] ->
            {more, {Cs, St, Off, Toks, SigilType, fun scan_string/5}};
        [Q1 | Ncs] ->
            case string_right_delimiter(Q1) of
                undefined ->
                    scan_error({illegal, string}, Off, Off, Cs);
                Q2 when is_integer(Q2) ->
                    scan_qstring(Ncs, St, Off, Toks, SigilType, Q1, Q2)
            end;
        [] ->
            {more, {Cs, St, Off, Toks, SigilType, fun scan_string/5}};
        eof ->
            scan_error({illegal, string}, Off, Off, Cs)
    end.

%% String delimiters are enumerated here.
%%
%% The $" and $' delimiters are also handled in scan1/5 above
%% to recognize regular strings and quoted atoms.
%% For triple-quoted strings it is hardcoded in scan_string/6
%% and scan_tqstring_lines/8 to look for $".
%%
-compile({inline, [string_right_delimiter/1]}).
string_right_delimiter(C) ->
    case C of
        $( ->
            $);
        $[ ->
            $];
        ${ ->
            $};
        $< ->
            $>;
        _ when
            C =:= $/;
            C =:= $|;
            C =:= $#;
            C =:= $`;
            %'
            C =:= $';
            %"
            C =:= $"
        ->
            C;
        _ ->
            undefined
    end.

% Triple-quoted String state
-record(tqs, {
    % Offset of first quote character
    offset,
    % atom() | {}
    sigil_type,
    % Number of quote characters in delimiter
    qs,
    % Ignore escape sequences?
    verbatim,
    % Quote character counter | undefined
    qn = undefined,
    % Scanned text (reversed)
    str = "",
    % Reverse list of reversed content lines
    content_r = [],
    % Current line accumulator
    acc = ""
}).
%%
%% Triple-quoted string (delimited by at least 3 double quote characters)
%%
%% Start delimiter may be followed only by white space.
%%
%% End delimiter may be preceeded only by white space, which defines
%% the indentation to strip from all content lines.
%%
scan_tqstring(Cs, St, Off, Toks, SigilType, Qs) ->
    scan_tqstring(Cs, St, Off, Toks, {SigilType, Qs}).
%%
%% Scan leading $" characters until we have them all, then scan lines
scan_tqstring(Cs, St, Off, Toks, {SigilType, Qs}) ->
    %"
    case scan_count(Cs, $", Qs) of
        {[], Nqs} ->
            {more, {[], St, Off, Toks, {SigilType, Nqs}, fun scan_tqstring/6}};
        {Ncs, Nqs} ->
            Verbatim =
                if
                    % binary(), non-verbatim
                    SigilType =:= 'b';
                    % string(), non-verbatim
                    SigilType =:= 's' ->
                        false;
                    % The rest are verbatim
                    true ->
                        true
                end,
            Tqs =
                #tqs{
                    offset = Off,
                    sigil_type = SigilType,
                    qs = Nqs,
                    verbatim = Verbatim,
                    %"
                    str = lists_duplicate(Nqs, $", "")
                },
            scan_tqstring_lines(Ncs, St, Nqs, Toks, Tqs)
    end.

%% Scan off characters that are C and count them
scan_count([C | Cs], C, N) -> scan_count(Cs, C, N + 1);
scan_count(Cs, _, N) -> {Cs, N}.

scan_tqstring_lines(Cs, St, Off, Toks, Tqs) ->
    #tqs{qn = Qn, str = Str, content_r = ContentR, acc = Acc} = Tqs,
    case
        scan_tqstring_lines(
            Cs, Tqs, Off, Str, Qn, ContentR, Acc
        )
    of
        {ok, Ncs, Noff, Nstr, NcontentR, IndentR} ->
            %% Last line - post process the content
            scan_tqstring_finish(
                Ncs,
                St,
                Noff,
                Toks,
                Tqs#tqs{str = Nstr, content_r = NcontentR, acc = IndentR}
            );
        {more, Ncs, Noff, Nstr, Nqn, NcontentR, Nacc} ->
            {more,
                {Ncs, St, Noff, Toks,
                    Tqs#tqs{
                        qn = Nqn, str = Nstr, content_r = NcontentR, acc = Nacc
                    },
                    fun scan_tqstring_lines/5}};
        {error, Ncs, Noff, Noff1, Error} ->
            Off0 = Tqs#tqs.offset,
            scan_error(
                Error,
                Noff + Off0,
                Noff1 + Off0,
                Ncs
            )
    end.

%% Inner loop that minimizes garbage creation
scan_tqstring_lines(Cs0, Tqs, Off, Str, Qn, ContentR, Acc) ->
    case Cs0 of
        [$\n = C | Cs] ->
            %% New line, restart searching for ending delimiter
            scan_tqstring_lines(
                Cs,
                Tqs,
                Off + 1,
                [C | Str],
                Tqs#tqs.qs,
                [[C | Acc] | ContentR],
                []
            );
        [C | Cs] ->
            if
                %"
                C =:= $", is_integer(Qn) ->
                    %% Possibly part of ending delimiter
                    Nstr = [C | Str],
                    Nacc = [C | Acc],
                    Noff = Off + 1,
                    if
                        Qn =:= 1 ->
                            %% Complete ending delimiter
                            IndentR = lists:nthtail(Tqs#tqs.qs, Nacc),
                            {ok, Cs, Noff, Nstr, ContentR, IndentR};
                        true ->
                            %% Collect and count this quote char
                            scan_tqstring_lines(
                                Cs,
                                Tqs,
                                Noff,
                                Nstr,
                                Qn - 1,
                                ContentR,
                                Nacc
                            )
                    end;
                ?WHITE_SPACE(C), is_integer(Qn) ->
                    %% White space while searching for ending delimiter
                    Nstr = [C | Str],
                    Nacc = [C | Acc],
                    Noff = Off + 1,
                    if
                        Qn =:= Tqs#tqs.qs ->
                            %% White space before first quote char
                            %% - just collect
                            scan_tqstring_lines(
                                Cs,
                                Tqs,
                                Noff,
                                Nstr,
                                Qn,
                                ContentR,
                                Nacc
                            );
                        true ->
                            %% White space after too few end quote chars
                            %% - stop searching for end quote chars
                            scan_tqstring_lines(
                                Cs,
                                Tqs,
                                Noff,
                                Nstr,
                                undefined,
                                ContentR,
                                Nacc
                            )
                    end;
                C =:= $\\, not Tqs#tqs.verbatim ->
                    case scan_escape(Cs, Off) of
                        more ->
                            {more, Cs, Off, Str, Qn, ContentR, Acc};
                        {error, Ncs, Error, N} ->
                            {error, Ncs, Off, Off + N, Error};
                        {eof, N} ->
                            scan_tqstring_eof(
                                [Acc | ContentR], Tqs, N + 1
                            );
                        {Val, ValStr, Ncs, N} ->
                            Nstr = lists:reverse(ValStr, [C | Str]),
                            scan_tqstring_lines(
                                Ncs,
                                Tqs,
                                N + 1,
                                Nstr,
                                undefined,
                                ContentR,
                                [Val | Acc]
                            )
                    end;
                ?UNICODE(C) ->
                    %% Not searching / stop searching for ending delimiter
                    scan_tqstring_lines(
                        Cs,
                        Tqs,
                        Off + char_byte_size(C),
                        [C | Str],
                        undefined,
                        ContentR,
                        [C | Acc]
                    );
                ?CHAR(C) ->
                    %% Illegal Unicode character
                    {error, Cs, Off, Off + 1, {illegal, character}}
            end;
        [] ->
            {more, Cs0, Off, Str, Qn, ContentR, Acc};
        eof ->
            scan_tqstring_eof([Acc | ContentR], Tqs, Off)
    end.

scan_tqstring_eof(ContentR, Tqs, Off) ->
    #tqs{offset = Off0, sigil_type = SigilType, qs = Qs} = Tqs,
    {error, eof, Off0 + length(Qs), Off,
        {unterminated,
            if
                SigilType =:= ?NO_SIGIL ->
                    {string, Qs};
                is_atom(SigilType) ->
                    {sigil, SigilType, Qs}
            end,
            string_head(ContentR)}}.

%% Strip last line newline,
%% get indentation definition from last line,
%% strip indentation from content lines,
%% check white space on first line after start delimiter,
%% create the string token,
%% done
%%
scan_tqstring_finish(Cs, St, Off, Toks, Tqs) ->
    %% IndentR :: Indentation characters, reversed
    %%
    #tqs{
        offset = Offset0,
        content_r = ContentR,
        acc = IndentR
    } = Tqs,
    NcontentR = strip_last_line_newline_r(ContentR),
    %%
    %% NcontentR is now the string's content lines
    %% including the characters after the opening quote sequence
    %% (the 0:th line?), in reversed order, each line reversed.
    %% Newline has been stripped from the last content line,
    %% all others has newline characters as from the input.
    case tqstring_finish(lists:reverse(IndentR), NcontentR, Off - 1) of
        Content when is_list(Content) ->
            #tqs{str = _Str, sigil_type = SigilType} = Tqs,
            Tok = {string, {Offset0, Offset0 + Off}, Content},
            scan_sigil_suffix(
                Cs, St, Off + Offset0, [Tok | Toks], SigilType
            );
        {Tag = indentation, ErrorOff} ->
            scan_error(
                Tag,
                Off,
                Off + ErrorOff,
                Cs
            );
        {Tag = white_space, N} ->
            scan_error(
                Tag,
                Off,
                Off + N + Tqs#tqs.qs + N,
                Cs
            )
    end.

%% Strip newline from the last line, but not if it is the only line
%%
strip_last_line_newline_r(ContentR = [_]) ->
    ContentR;
strip_last_line_newline_r([LastLineR | ContentR]) ->
    [strip_newline_r(LastLineR) | ContentR].

strip_newline_r("\n\r" ++ Rcs) -> Rcs;
strip_newline_r("\n" ++ Rcs) -> Rcs.

%% Loop from last to first line and remember the last error,
%% so the last error that is found will be the one reported,
%% that is: the first in the string.
%%
%% Build the string content one line at the time by first
%% prepending the line to Content and then spripping
%% the defined indentation.
%%
%% For the first (0:th) line, strip the newline and then
%% check that it contains only white space
%%
tqstring_finish(Indent, ContentR, Line) ->
    tqstring_finish(Indent, ContentR, Line, undefined, "").
%%
tqstring_finish(_Indent, [FirstLineR], _Line, Error, Content) ->
    NfirstLineR = strip_newline_r(FirstLineR),
    FirstLine = lists:reverse(NfirstLineR),
    %% First line; check that it is all white space
    case check_white_space(FirstLine) of
        ok ->
            if
                Error =:= undefined ->
                    Content;
                true ->
                    Error
            end;
        N ->
            {white_space, N}
    end;
tqstring_finish(
    Indent, [StringR | StringsR], Line, Error, Content
) ->
    case strip_indent(Indent, lists:reverse(StringR, Content)) of
        Ncontent when is_list(Ncontent) ->
            tqstring_finish(Indent, StringsR, Line - 1, Error, Ncontent);
        ErrorCol when is_integer(ErrorCol) ->
            Nerror = {indentation, Line, ErrorCol},
            tqstring_finish(Indent, StringsR, Line - 1, Nerror, "")
    end.

%% Strip the defined indentation from the string content
%%
strip_indent(Indent, Cs) ->
    case Cs of
        %% Allow empty content lines to have no indentation
        "\r\n" ++ _ -> Cs;
        "\n" ++ _ -> Cs;
        % The last newline is stripped
        "" -> Cs;
        _ -> strip_indent(Indent, Cs, 1)
    end.
%%
strip_indent([C | Indent], [C | Cs], Col) ->
    % Strip
    strip_indent(Indent, Cs, Col + 1);
% Done
strip_indent([], Cs, _) ->
    Cs;
% Incorrect indentation
strip_indent(_, _, Col) ->
    Col.

%% Check that all characters are white space and return 'ok',
%% or return the number of white space characters
check_white_space(Cs) ->
    check_white_space(Cs, 0).
%%
check_white_space([], _) ->
    ok;
check_white_space([C | Cs], N) ->
    if
        ?WHITE_SPACE(C) ->
            check_white_space(Cs, N + 1);
        true ->
            N
    end.

-record(qstring, {offset = 1, sigil_type, q1, q2, str = undefined, wcs = ""}).
%%
%% Quoted string
%%
scan_qstring(Cs, St, Off, Toks, SigilType, Q1, Q2) ->
    if
        SigilType =:= ?NO_SIGIL;
        % The vanilla (default) sigil
        SigilType =:= '';
        % binary() with escape sequences
        SigilType =:= 'b';
        % string() with escape sequences
        SigilType =:= 's' ->
            % Quote character
            scan_qstring(
                Cs,
                St,
                Off,
                Toks,
                #qstring{
                    sigil_type = SigilType,
                    q1 = Q1,
                    q2 = Q2
                }
            );
        % Verbatim string
        true ->
            scan_vstring(Cs, St, Off, Toks, SigilType, Q1, Q2)
    end.

scan_qstring(
    Cs,
    #erl_scan{} = St,
    Off,
    Toks,
    #qstring{offset = N0, q2 = Q2, str = Str, wcs = Wcs} = Qstring
) ->
    case scan_string0(Cs, N0, Q2, Str, Wcs) of
        {done, Ncs, Nwcs, N} ->
            #qstring{
                sigil_type = SigilType
            } = Qstring,
            Tok = {string, {Off, Off + N}, lists:reverse(Nwcs)},
            scan_sigil_suffix(Ncs, St, Off + N, [Tok | Toks], SigilType);
        {more, {Ncs, N, Nstr, Nwcs}} ->
            Nqstring = Qstring#qstring{offset = N, str = Nstr, wcs = Nwcs},
            {more, {Ncs, St, Off, Toks, Nqstring, fun scan_qstring/5}};
        {eof, {Ncs, N, Nwcs}} ->
            #qstring{
                sigil_type = SigilType,
                q1 = Q1
            } = Qstring,
            scan_error(
                {unterminated,
                    if
                        SigilType =:= ?NO_SIGIL ->
                            string;
                        is_atom(SigilType) ->
                            {sigil, SigilType, Q1, Q2}
                    end,
                    string_head([Nwcs])},
                Off,
                Off + N,
                Ncs
            );
        {{error, _, _}, _Ncs} = Error ->
            Error
    end.

scan_sigil_suffix(Cs, St, Off, Toks, ?NO_SIGIL) ->
    scan_string_concat(Cs, St, Off, Toks, {"", 0});
scan_sigil_suffix(Cs, St, Off, Toks, SigilType) when
    % Sigil string - scan suffix
    is_atom(SigilType)
->
    scan_sigil_suffix(Cs, St, Off, Toks, {"", 0});
%%
scan_sigil_suffix(Cs, St, Off, Toks, {Wcs, N0}) when is_list(Wcs) ->
    case scan_name(Cs, Wcs, N0) of
        {more, Nwcs, N} ->
            {more, {[], St, Off, Toks, {Nwcs, N}, fun scan_sigil_suffix/5}};
        {done, Nwcs, Ncs, N} ->
            Type = sigil_suffix,
            Noff = Off + N,
            Suffix = lists:reverse(Nwcs),
            try list_to_atom(Suffix) of
                A when is_atom(A) ->
                    Tok = {Type, {Off, Noff}, Suffix},
                    scan_string_concat(
                        Ncs, St, Noff, [Tok | Toks], {Suffix, 0}
                    )
            catch
                _:_ ->
                    scan_error({illegal, Type}, Off, Noff, Ncs)
            end
    end.

scan_string_concat(Cs, St, Off, Toks, {"", N}) ->
    case Cs of
        [$" | _] ->
            scan_error(string_concat, Off, Off + N, Cs);
        [] ->
            {more, {Cs, St, Off + N, Toks, {[], N}, fun scan_string_concat/5}};
        _ ->
            scan1(Cs, St, Off + N, Toks)
    end;
scan_string_concat(Cs, St, Off, Toks, {SigilSuffix, N}) when
    is_list(SigilSuffix)
->
    scan1(Cs, St, Off + N, Toks).

-record(vstring, {offset, sigil_type, q1, q2, wcs = ""}).
%%
%% Verbatim quoted string (not triple-quoted)
%%
scan_vstring(Cs, St, Off, Toks, SigilType, Q1, Q2) ->
    Vstring =
        #vstring{
            offset = Off,
            sigil_type = SigilType,
            q1 = Q1,
            q2 = Q2
        },
    scan_vstring(Cs, St, Off + 1, Toks, Vstring).

scan_vstring(
    Cs,
    #erl_scan{} = St,
    Off,
    Toks,
    #vstring{q2 = Q2, wcs = Wcs} = Vstring
) ->
    case scan_vstring(Cs, Q2, Off, Wcs) of
        {ok, {Ncs, Noff, Nwcs}} ->
            #vstring{
                offset = Off0,
                sigil_type = SigilType
            } = Vstring,
            Tok = {string, Off0, lists:reverse(Nwcs)},
            scan_sigil_suffix(Ncs, St, Noff, [Tok | Toks], SigilType);
        {more, {Ncs, Noff, Nwcs}} ->
            Nvstring = Vstring#vstring{wcs = Nwcs},
            {more, {Ncs, St, Noff, Toks, Nvstring, fun scan_vstring/5}};
        {eof, {Ncs, Noff, Nwcs}} ->
            #vstring{
                offset = Off0,
                sigil_type = SigilType,
                q1 = Q1
            } = Vstring,
            scan_error(
                {unterminated,
                    if
                        SigilType =:= ?NO_SIGIL ->
                            string;
                        is_atom(SigilType) ->
                            {sigil, SigilType, Q1, Q2}
                    end,
                    string_head([Nwcs])},
                Off0 + 1,
                Noff,
                Ncs
            );
        {{error, _, _}, _Ncs} = Error ->
            Error
    end.

scan_vstring(Cs, Q, Off, Wcs) ->
    case Cs of
        [Q | Ncs] ->
            {ok, {Ncs, Off + 1, Wcs}};
        [$\n = C | Ncs] ->
            scan_vstring(Ncs, Q, Off + 1, [C | Wcs]);
        [C | Ncs] when ?UNICODE(C) ->
            scan_vstring(Ncs, Q, Off + 1, [C | Wcs]);
        [C | Ncs] when ?CHAR(C) ->
            scan_error(
                {illegal, character}, Off, Off + 1, Ncs
            );
        [] ->
            {more, {Cs, Off, Wcs}};
        eof ->
            {eof, {Cs, Off, Wcs}}
    end.

-record(qatom, {offset = 1, str = undefined, wcs = ""}).

scan_qatom(Cs, St, Off, Toks) ->
    scan_qatom(Cs, St, Off, Toks, #qatom{}).

scan_qatom(
    Cs,
    #erl_scan{} = St,
    Off,
    Toks,
    #qatom{offset = Offset, str = Str, wcs = Wcs} = Qatom
) ->
    %'
    C = $',
    case scan_string0(Cs, Offset, C, Str, Wcs) of
        {done, Ncs, Nwcs, N} ->
            try list_to_atom(lists:reverse(Nwcs)) of
                A when is_atom(A) ->
                    tok3(Ncs, St, Off, Toks, atom, A, N)
            catch
                _:_ ->
                    scan_error(
                        {illegal, atom}, Off, Off + N, Ncs
                    )
            end;
        {more, {Ncs, N, Nstr, Nwcs}} ->
            Nqatom = Qatom#qatom{str = Nstr, wcs = Nwcs, offset = N},
            {more, {Ncs, St, Off, Toks, Nqatom, fun scan_qatom/5}};
        {eof, {Ncs, Noff, Nwcs}} ->
            #qatom{offset = Off0} = Qatom,
            scan_error(
                {unterminated, atom, string_head([Nwcs])},
                Off0 + 1,
                Noff,
                Ncs
            );
        {{error, _, _}, _Ncs} = Error ->
            Error
    end.

string_head(ContentR) ->
    string:slice(lists_foldl_reverse(ContentR, ""), 0, 16).

scan_string0(Cs, N, Q, [], Wcs) ->
    scan_string_col(Cs, N, Q, Wcs);
scan_string0(Cs, N, Q, Str, Wcs) ->
    scan_string1(Cs, N, Q, Str, Wcs).

scan_string_col([Q | Cs], N, Q, Wcs) ->
    {done, Cs, Wcs, N + 1};
scan_string_col([$\n = C | Cs], N, Q, Wcs) ->
    scan_string_col(Cs, N + 1, Q, [C | Wcs]);
scan_string_col([C | Cs], N, Q, Wcs) when C =/= $\\, ?UNICODE(C) ->
    scan_string_col(Cs, N + char_byte_size(C), Q, [C | Wcs]);
scan_string_col(Cs, N, Q, Wcs) ->
    scan_string1(Cs, N, Q, Wcs, Wcs).

%% Note: in those cases when a 'char_error' tuple is returned below it
%% is tempting to skip over characters up to the first Q character,
%% but then the end location of the error tuple would not correspond
%% to the start location of the returned Rest string. (Maybe the end
%% location could be modified, but that too is ugly.)
scan_string1([Q | Cs], N, Q, _StrW, Wcs) ->
    {done, Cs, Wcs, N + 1};
scan_string1([$\n = C | Cs], N, Q, Str, Wcs) ->
    scan_string1(Cs, N + 1, Q, [C | Str], [C | Wcs]);
scan_string1([$\\ | Cs] = Cs0, N0, Q, Str, Wcs) ->
    case scan_escape(Cs, N0 + 1) of
        more ->
            {more, {Cs0, N0, Str, Wcs}};
        {error, Ncs, Error, N} ->
            scan_error(Error, N0, N, Ncs);
        {eof = Ncs, N} ->
            {eof, {Ncs, N, lists:reverse(Wcs)}};
        {Val, ValStr, Ncs, N} ->
            Nstr = lists:reverse(ValStr, [$\\ | Str]),
            Nwcs = [Val | Wcs],
            scan_string1(Ncs, N, Q, Nstr, Nwcs)
    end;
scan_string1([C | Cs], N, Q, Str, Wcs) when ?UNICODE(C) ->
    scan_string1(Cs, N + char_byte_size(C), Q, [C | Str], [C | Wcs]);
scan_string1([C | Cs], N, _Q, _Str, _Wcs) when ?CHAR(C) ->
    scan_error({illegal, character}, N, N + char_byte_size(C), Cs);
scan_string1([] = Cs, N, _Q, Str, Wcs) ->
    {more, {Cs, N, Str, Wcs}};
scan_string1(eof = Cs, N, _Q, _Str, Wcs) ->
    {eof, {Cs, N, Wcs}}.

-define(OCT(C), (is_integer(C) andalso $0 =< C andalso C =< $7)).
-define(HEX(C),
    (is_integer(C) andalso
        (C >= $0 andalso C =< $9 orelse
            C >= $A andalso C =< $F orelse
            C >= $a andalso C =< $f))
).

%% \<1-3> octal digits
scan_escape([O1, O2, O3 | Cs], N) when ?OCT(O1), ?OCT(O2), ?OCT(O3) ->
    Val = (O1 * 8 + O2) * 8 + O3 - 73 * $0,
    {Val, [O1, O2, O3], Cs, N + 3};
scan_escape([O1, O2], _N) when ?OCT(O1), ?OCT(O2) ->
    more;
scan_escape([O1, O2 | Cs], N) when ?OCT(O1), ?OCT(O2) ->
    Val = (O1 * 8 + O2) - 9 * $0,
    {Val, [O1, O2], Cs, N + 2};
scan_escape([O1], _N) when ?OCT(O1) ->
    more;
scan_escape([O1 | Cs], N) when ?OCT(O1) ->
    {O1 - $0, [O1], Cs, N + 1};
%% \x{<hex digits>}
scan_escape([$x, ${ | Cs], N) ->
    scan_hex(Cs, N + 2, []);
scan_escape([$x], _N) ->
    more;
scan_escape([$x | eof], N) ->
    {eof, N + 1};
%% \x<2> hexadecimal digits
scan_escape([$x, H1, H2 | Cs], N) when ?HEX(H1), ?HEX(H2) ->
    Val = erlang:list_to_integer([H1, H2], 16),
    {Val, [$x, H1, H2], Cs, N + 3};
scan_escape([$x, H1], _N) when ?HEX(H1) ->
    more;
scan_escape([$x | Cs], N) ->
    {error, Cs, {illegal, character}, N + 1};
%% \^X -> Control-X
scan_escape([$^ = C0, C | Cs], N) when ?CHAR(C) ->
    case caret_char_code(C) of
        error ->
            {error, [C | Cs], {illegal, character}, N + 1};
        Code ->
            {Code, [C0, C], Cs, N + 2}
    end;
scan_escape([$^], _N) ->
    more;
scan_escape([$^ | eof], N) ->
    {eof, N + 1};
scan_escape([$\n = C | Cs], N) ->
    {C, [C], Cs, N + 1};
scan_escape([C0 | Cs], N) when ?UNICODE(C0) ->
    C = escape_char(C0),
    {C, [C0], Cs, N + char_byte_size(C0)};
scan_escape([C | Cs], N) when ?CHAR(C) ->
    {error, Cs, {illegal, character}, N + char_byte_size(C)};
scan_escape([], _N) ->
    more;
scan_escape(eof, N) ->
    {eof, N}.

scan_hex([C | Cs], N, Wcs) when ?HEX(C) ->
    scan_hex(Cs, N + 1, [C | Wcs]);
scan_hex(Cs, N, Wcs) ->
    scan_hex_end(Cs, N, Wcs, "x{").

scan_hex_end([$} | Cs], N, [], _Str) ->
    %% Empty escape sequence.
    {error, Cs, {illegal, character}, N + 1};
scan_hex_end([$} | Cs], N, Wcs0, Str0) ->
    Wcs = lists:reverse(Wcs0),
    try list_to_integer(Wcs, 16) of
        Val when ?UNICODE(Val) ->
            {Val, Str0 ++ Wcs ++ [$}], Cs, N + 1};
        _Val ->
            {error, Cs, {illegal, character}, N + 1}
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            {error, Cs, {illegal, character}, N + 1}
    end;
scan_hex_end([], _N, _Wcs, _Str0) ->
    more;
scan_hex_end(eof, N, _Wcs, _Str0) ->
    {eof, N};
scan_hex_end(Cs, N, _Wcs, _Str0) ->
    {error, Cs, {illegal, character}, N}.

% \n = LF
escape_char($n) -> $\n;
% \r = CR
escape_char($r) -> $\r;
% \t = TAB
escape_char($t) -> $\t;
% \v = VT
escape_char($v) -> $\v;
% \b = BS
escape_char($b) -> $\b;
% \f = FF
escape_char($f) -> $\f;
% \e = ESC
escape_char($e) -> $\e;
% \s = SPC
escape_char($s) -> $\s;
% \d = DEL
escape_char($d) -> $\d;
escape_char(C) -> C.

caret_char_code($?) -> 16#7f;
caret_char_code(C) when $@ =< C, C =< $_; $a =< C, C =< $z -> C band 16#1f;
caret_char_code(_) -> error.

scan_number(Cs, #erl_scan{} = St, Off, Toks, {Ncs, Us}) ->
    scan_number(Cs, St, Off, Toks, Ncs, Us).

scan_number([C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_number(Cs, St, Off, Toks, [C | Ncs], Us);
scan_number([$_, Next | Cs], St, Off, Toks, [Prev | _] = Ncs, _Us) when
    ?DIGIT(Next) andalso ?DIGIT(Prev)
->
    scan_number(Cs, St, Off, Toks, [Next, $_ | Ncs], with_underscore);
scan_number([$_] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_number/5}};
scan_number([$., C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Off, Toks, [C, $. | Ncs], Us);
scan_number([$.] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_number/5}};
scan_number([$# | Cs] = Cs0, St, Off, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    try list_to_integer(remove_digit_separators(Ncs, Us)) of
        B when is_integer(B), 2 =< B, B =< 1 + $Z - $A + 10 ->
            Bcs = Ncs ++ [$#],
            scan_based_int(Cs, St, Off, Toks, B, [], Bcs, no_underscore);
        B when is_integer(B) ->
            Len = length(Ncs),
            scan_error({base, B}, Off, Off + Len, Cs0)
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            scan_error({illegal, base}, Off, Off, Cs0)
    end;
scan_number([] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_number/5}};
scan_number(Cs, St, Off, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    try list_to_integer(remove_digit_separators(Ncs, Us), 10) of
        N ->
            tok3(Cs, St, Off, Toks, integer, N, length(Ncs))
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            scan_error({illegal, integer}, Off, Off + length(Ncs), Cs)
    end.

remove_digit_separators(Number, no_underscore) ->
    Number;
remove_digit_separators(Number, with_underscore) ->
    [C || C <- Number, C =/= $_].

-define(BASED_DIGIT(C, B),
    (is_integer(C) andalso
        ((?DIGIT(C) andalso C < $0 + B) orelse
            (C >= $A andalso B > 10 andalso C < $A + B - 10) orelse
            (C >= $a andalso B > 10 andalso C < $a + B - 10)))
).

scan_based_int(Cs, #erl_scan{} = St, Off, Toks, {B, NCs, BCs, Us}) when
    is_integer(B), 2 =< B, B =< 1 + $Z - $A + 10
->
    scan_based_int(Cs, St, Off, Toks, B, NCs, BCs, Us).

scan_based_int([C | Cs], St, Off, Toks, B, Ncs, Bcs, Us) when
    ?BASED_DIGIT(C, B)
->
    scan_based_int(Cs, St, Off, Toks, B, [C | Ncs], Bcs, Us);
scan_based_int([$_, Next | Cs], St, Off, Toks, B, [Prev | _] = Ncs, Bcs, _Us) when
    ?BASED_DIGIT(Next, B) andalso ?BASED_DIGIT(Prev, B)
->
    scan_based_int(
        Cs,
        St,
        Off,
        Toks,
        B,
        [Next, $_ | Ncs],
        Bcs,
        with_underscore
    );
scan_based_int([$_] = Cs, St, Off, Toks, B, NCs, BCs, Us) ->
    {more, {Cs, St, Off, Toks, {B, NCs, BCs, Us}, fun scan_based_int/5}};
scan_based_int([] = Cs, St, Off, Toks, B, NCs, BCs, Us) ->
    {more, {Cs, St, Off, Toks, {B, NCs, BCs, Us}, fun scan_based_int/5}};
scan_based_int(Cs, _St, Off, _Toks, _B, [], Bcs, _Us) ->
    %% No actual digits following the base.
    Len = length(Bcs),
    scan_error({illegal, integer}, Off, Off + Len, Cs);
scan_based_int(Cs, St, Off, Toks, B, Ncs0, [_ | _] = Bcs, Us) ->
    Ncs = lists:reverse(Ncs0),
    Len = length(Bcs) + length(Ncs),
    try list_to_integer(remove_digit_separators(Ncs, Us), B) of
        N ->
            tok3(Cs, St, Off, Toks, integer, N, Len)
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            scan_error({illegal, integer}, Off, Off + Len, Cs)
    end.

scan_fraction(Cs, #erl_scan{} = St, Off, Toks, {Ncs, Us}) ->
    scan_fraction(Cs, St, Off, Toks, Ncs, Us).

scan_fraction([C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Off, Toks, [C | Ncs], Us);
scan_fraction([$_, Next | Cs], St, Off, Toks, [Prev | _] = Ncs, _Us) when
    ?DIGIT(Next) andalso ?DIGIT(Prev)
->
    scan_fraction(Cs, St, Off, Toks, [Next, $_ | Ncs], with_underscore);
scan_fraction([$_] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_fraction/5}};
scan_fraction([E | Cs], St, Off, Toks, Ncs, Us) when E =:= $e; E =:= $E ->
    scan_exponent_sign(Cs, St, Off, Toks, [E | Ncs], Us);
scan_fraction([] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_fraction/5}};
scan_fraction(Cs, St, Off, Toks, Ncs, Us) ->
    float_end(Cs, St, Off, Toks, Ncs, Us).

scan_exponent_sign(Cs, #erl_scan{} = St, Off, Toks, {Ncs, Us}) ->
    scan_exponent_sign(Cs, St, Off, Toks, Ncs, Us).

scan_exponent_sign([C | Cs], St, Off, Toks, Ncs, Us) when
    C =:= $+; C =:= $-
->
    scan_exponent(Cs, St, Off, Toks, [C | Ncs], Us);
scan_exponent_sign([] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_exponent_sign/5}};
scan_exponent_sign(Cs, St, Off, Toks, Ncs, Us) ->
    scan_exponent(Cs, St, Off, Toks, Ncs, Us).

scan_exponent(Cs, #erl_scan{} = St, Off, Toks, {Ncs, Us}) ->
    scan_exponent(Cs, St, Off, Toks, Ncs, Us).

scan_exponent([C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_exponent(Cs, St, Off, Toks, [C | Ncs], Us);
scan_exponent([$_, Next | Cs], St, Off, Toks, [Prev | _] = Ncs, _) when
    ?DIGIT(Next) andalso ?DIGIT(Prev)
->
    scan_exponent(Cs, St, Off, Toks, [Next, $_ | Ncs], with_underscore);
scan_exponent([$_] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_exponent/5}};
scan_exponent([] = Cs, St, Off, Toks, Ncs, Us) ->
    {more, {Cs, St, Off, Toks, {Ncs, Us}, fun scan_exponent/5}};
scan_exponent(Cs, St, Off, Toks, Ncs, Us) ->
    float_end(Cs, St, Off, Toks, Ncs, Us).

float_end(Cs, St, Off, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    try list_to_float(remove_digit_separators(Ncs, Us)) of
        F ->
            tok3(Cs, St, Off, Toks, float, F, length(Ncs))
    catch
        _:_ ->
            scan_error({illegal, float}, Off, Off + length(Ncs), Cs)
    end.

skip_comment_fun(Cs, #erl_scan{} = St, Off, Toks, N) ->
    skip_comment(Cs, St, Off, Toks, N).

skip_comment([C | Cs], St, Off, Toks, N) when C =/= $\n, ?CHAR(C) ->
    case ?UNICODE(C) of
        true ->
            skip_comment(Cs, St, Off, Toks, N + char_byte_size(C));
        false ->
            scan_error({illegal, character}, Off, Off + N + char_byte_size(C), Cs)
    end;
skip_comment([] = Cs, St, Off, Toks, N) ->
    {more, {Cs, St, Off, Toks, N, fun skip_comment_fun/5}};
skip_comment(Cs, St, Off, Toks, N) ->
    scan1(Cs, St, Off + N, Toks).

tok2(Cs, St, Off, Toks, Item, Len) ->
    Token = {Item, {Off, Off + Len}},
    scan1(Cs, St, Off + Len, [Token | Toks]).

tok3(Cs, St, Off, Toks, Item, Value, Len) ->
    Token = {Item, {Off, Off + Len}, Value},
    scan1(Cs, St, Off + Len, [Token | Toks]).

scan_error(Error, StartOff, EndOff, Rest) ->
    {{error, {{StartOff, EndOff}, ?MODULE, Error}, EndOff}, Rest}.

%% lists:duplicate/3 (not exported)
lists_duplicate(0, _, L) -> L;
lists_duplicate(N, X, L) -> lists_duplicate(N - 1, X, [X | L]).

%% lists:foldl/3 over lists:reverse/2
lists_foldl_reverse(Lists, Acc) ->
    lists:foldl(fun lists:reverse/2, Acc, Lists).

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') -> true;
reserved_word('cond') -> true;
reserved_word('catch') -> true;
reserved_word('andalso') -> true;
reserved_word('orelse') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word('maybe') -> true;
reserved_word('else') -> true;
reserved_word(_) -> false.

%% We skip over CR to match what ELP is expecting
char_byte_size($\r) -> 0;
char_byte_size(C) ->
    %% TODO: more efficient implementation
    byte_size(<<C/utf8>>).
