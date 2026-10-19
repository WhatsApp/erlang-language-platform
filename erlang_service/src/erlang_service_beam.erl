%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format

%% Serves parse requests for modules ELP cannot parse itself: those compiled
%% to Erlang forms by another compiler, such as Elixir's, which records the
%% forms in the BEAM file. The request names the application's `ebin`
%% directory and the module wanted, since one source file can define several
%% modules, and `beam_lib` rebuilds the forms from the debug info. That takes
%% the compiler's debug-info backend, `elixir_erl` for Elixir, which is on the
%% code path once ELP adds the project's dependencies to it, the compiler's
%% application among them.
%%
%% A `.beam` file stands in for a module with no source, such as the Erlang
%% modules of the Elixir compiler that define types: it is read as it is, and
%% its forms get no positions.
%%
%% `run/2` has the contract of `erlang_service_lint:run/2`.
-module(erlang_service_beam).

-export([run/2]).

-type forms() :: [erl_parse:abstract_form() | erl_parse:form_info()].

-spec run(integer(), [term()]) -> {ok, [{binary(), iodata()}]} | {error, binary()}.
run(_Id, [FileName, _FileId, Options, FileText, PostProcess, _Deterministic]) ->
    case read(FileName, Options, FileText) of
        {ok, Forms} ->
            {ok, [{<<"AST">>, PostProcess(Forms, FileName)}]};
        {error, Reason} ->
            Message = io_lib:format("~ts: ~ts", [FileName, describe(Reason)]),
            {error, unicode:characters_to_binary(Message)}
    end.

%% The forms of the module the request names, read from its BEAM file, with
%% every annotation rewritten into the byte range of `FileText` ELP expects.
-spec read(file:filename(), [term()], binary()) -> {ok, forms()} | {error, term()}.
read(FileName, Options, FileText) ->
    maybe
        {ok, Beam, Text} ?= locate(FileName, Options, FileText),
        {ok, Forms0} ?= forms(Beam),
        Forms1 = reject_generated_functions(Forms0),
        Forms2 = to_byte_ranges(Forms1, Text),
        {ok, insert_metadata(Forms2, Options)}
    end.

%% The BEAM file to read, and the text whose byte offsets its forms get: none
%% for a `.beam` file itself, which is read as it is.
-spec locate(file:filename(), [term()], binary()) ->
    {ok, file:filename(), binary()} | {error, term()}.
locate(FileName, Options, FileText) ->
    case filename:extension(FileName) of
        ".beam" ->
            {ok, FileName, <<>>};
        _ ->
            Ebin = proplists:get_value(ebin, Options),
            case proplists:get_value(module, Options) of
                undefined ->
                    {error, no_module};
                Module ->
                    Beam = filename:join(Ebin, atom_to_list(Module) ++ ".beam"),
                    case filelib:is_regular(Beam) of
                        true -> {ok, Beam, FileText};
                        false -> {error, {beam_not_found, Beam}}
                    end
            end
    end.

%% The Erlang abstract forms recorded in a BEAM file.
-spec forms(file:filename()) -> {ok, forms()} | {error, term()}.
forms(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            {ok, Forms};
        {ok, {_Module, [{abstract_code, no_abstract_code}]}} ->
            {error, {no_debug_info, Beam}};
        {error, beam_lib, Reason} ->
            {error, {beam_lib, Reason}}
    end.

%% The functions the compiler generates are not the user's code, and what
%% eqWAlizer finds in their bodies, which rely on dynamic typing, has nowhere
%% to be reported: those at line 0, such as `__info__/1`, and those with names
%% reserved for the compiler, such as `__struct__/1`, whose bodies alone are
%% generated. Their specs and exports stay, so calls to them are checked.
-spec reject_generated_functions(forms()) -> forms().
reject_generated_functions(Forms) ->
    [Form || Form <- Forms, not generated_function(Form)].

-spec generated_function(erl_parse:abstract_form() | erl_parse:form_info()) -> boolean().
generated_function({function, Anno, Name, _Arity, _Clauses}) ->
    erl_anno:line(Anno) =:= 0 orelse reserved_name(Name);
generated_function(_Form) ->
    false.

%% `__name__`, as Elixir reserves for the compiler.
-spec reserved_name(atom()) -> boolean().
reserved_name(Name) ->
    case atom_to_list(Name) of
        "__" ++ Rest -> lists:suffix("__", Rest);
        _ -> false
    end.

%% ELP's own parser annotates every node with `{StartByte, EndByte}` instead
%% of a line and column, and eqWAlizer places its diagnostics by those. The
%% Elixir compiler records only where a node starts, so each range is one byte
%% long. Line 0 marks generated code and maps to the start of the text; a line
%% past the end maps to the end of the text.
-spec to_byte_ranges(forms(), binary()) -> forms().
to_byte_ranges(Forms, Text) ->
    LineStarts = line_starts(Text),
    Size = byte_size(Text),
    Translate = fun(Anno) ->
        Start = offset(erl_anno:location(Anno), Text, LineStarts, Size),
        {Start, min(Start + 1, Size)}
    end,
    [erl_parse:map_anno(Translate, Form) || Form <- Forms].

-spec offset(erl_anno:location(), binary(), tuple(), non_neg_integer()) -> non_neg_integer().
offset(Line, Text, LineStarts, Size) when is_integer(Line) ->
    offset({Line, 1}, Text, LineStarts, Size);
offset({Line, _Column}, _Text, _LineStarts, _Size) when Line < 1 ->
    0;
offset({Line, _Column}, _Text, LineStarts, Size) when Line > tuple_size(LineStarts) ->
    Size;
offset({Line, Column}, Text, LineStarts, Size) ->
    LineStart = element(Line, LineStarts),
    Rest = binary_part(Text, LineStart, Size - LineStart),
    LineStart + column_bytes(Rest, Column - 1).

%% The byte length of the first `Count` characters of the line starting
%% `Text`, or of the whole line when it is shorter.
-spec column_bytes(binary(), integer()) -> non_neg_integer().
column_bytes(_Text, Count) when Count =< 0 ->
    0;
column_bytes(<<>>, _Count) ->
    0;
column_bytes(<<$\n, _/binary>>, _Count) ->
    0;
column_bytes(<<_/utf8, Rest/binary>> = Text, Count) ->
    byte_size(Text) - byte_size(Rest) + column_bytes(Rest, Count - 1);
column_bytes(<<_, Rest/binary>>, Count) ->
    1 + column_bytes(Rest, Count - 1).

%% The byte offset of every line start, by line number from 1.
-spec line_starts(binary()) -> tuple().
line_starts(Text) ->
    list_to_tuple([0 | [Pos + 1 || {Pos, 1} <- binary:matches(Text, <<"\n">>)]]).

-spec insert_metadata(forms(), [term()]) -> forms().
insert_metadata(Forms, Options) ->
    case proplists:get_value(elp_metadata, Options) of
        undefined -> Forms;
        Metadata -> elp_metadata:insert_metadata(Metadata, Forms)
    end.

-spec describe(term()) -> io_lib:chars().
describe(no_module) ->
    "no module named for the file";
describe({beam_not_found, Beam}) ->
    io_lib:format("BEAM file not found: ~ts (is the project compiled?)", [Beam]);
describe({no_debug_info, Beam}) ->
    io_lib:format("~ts has no debug info", [Beam]);
describe({beam_lib, {missing_backend, Beam, Backend}}) ->
    io_lib:format(
        "the debug info of ~ts needs the ~p module, which is not on the code path "
        "(is the compiler's application listed as a dependency, with its ebin?)",
        [Beam, Backend]
    );
describe({beam_lib, Reason}) ->
    io_lib:format("beam_lib: ~p", [Reason]).
