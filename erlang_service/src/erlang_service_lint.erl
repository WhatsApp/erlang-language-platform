%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service_lint).

-export([run/1]).

run([FileName, Options0, OverrideOptions, PostProcess, Deterministic]) ->
    Options1 =
        case Deterministic of
            true ->
                % Force determinism.
                % The added options mirrors https://fburl.com/code/hwv3vzl5
                [{deterministic, true}, {source_name, filename:basename(FileName)} | Options0];
            false ->
                Options0
        end,
    %% Match WASERVER/erl/rebar.config.script erl_opts
    Options2 = [nowarn_underscore_match | Options1],
    %% TODO workaround to enable parsing third-party deps
    %% remove it after merge of https://github.com/jlouis/graphql-erlang/pull/225
    Options3 =
        case filename:basename(FileName) of
            "graphql_execute.erl" ->
                [{no_auto_import, [{alias, 1}]} | Options2];
            _ ->
                Options2
        end,
    case extract_forms(FileName, Options3) of
        {ok, Forms0} ->
            Transforms0 = proplists:get_value(parse_transforms, Options3, []),
            {Transforms, Forms1} = collect_parse_transforms(Forms0, [], Transforms0),
            Transform = fun(Mod, Forms) -> transform(Mod, Forms, Options3) end,
            Forms2 = lists:foldl(Transform, Forms1, Transforms),
            Forms3 =
                case proplists:get_value(elp_metadata, Options3) of
                    undefined ->
                        Forms2;
                    ElpMetadata ->
                        elp_metadata:insert_metadata(ElpMetadata, Forms2)
                end,
            case lint_file(Forms3, FileName, Options3, OverrideOptions) of
                {ok, []} ->
                    {Stub, AST, FILES} = partition_stub(Forms3),
                    ResultStub = PostProcess(Stub, FileName),
                    ResultAST = PostProcess(AST, FileName),
                    ResultFILES = unicode:characters_to_binary(FILES),
                    {ok, [{<<"AST">>, ResultAST}, {<<"STU">>, ResultStub}, {<<"FIL">>, ResultFILES}]};
                {ok, Warnings} ->
                    {Stub, AST, FILES} = partition_stub(Forms3),
                    ResultStub = PostProcess(Stub, FileName),
                    ResultAST = PostProcess(AST, FileName),
                    ResultFILES = unicode:characters_to_binary(FILES),
                    FormattedWarnings = format_errors(Forms3, FileName, Warnings),
                    {ok, [{<<"AST">>, ResultAST},
                          {<<"STU">>, ResultStub},
                          {<<"FIL">>, ResultFILES},
                          {<<"WAR">>, FormattedWarnings}]};
                {error, Errors, Warnings} ->
                    {Stub, AST, FILES} = partition_stub(Forms3),
                    ResultStub = PostProcess(Stub, FileName),
                    ResultAST = PostProcess(AST, FileName),
                    ResultFILES = unicode:characters_to_binary(FILES),
                    FormattedErrors = format_errors(Forms3, FileName, Errors),
                    FormattedWarnings = format_errors(Forms3, FileName, Warnings),
                    {ok, [
                        {<<"AST">>, ResultAST},
                        {<<"STU">>, ResultStub},
                        {<<"FIL">>, ResultFILES},
                        {<<"ERR">>, FormattedErrors},
                        {<<"WAR">>, FormattedWarnings}
                    ]}
            end;
        {error, Reason} ->
            Msg = unicode:characters_to_binary(
                file:format_error(Reason)
            ),
            {error, Msg}
    end.

lint_file(Forms, FileName, Options0, OverrideOptions) ->
    Options =
        case filename:extension(FileName) of
            ".hrl" ->
                [nowarn_unused_function, nowarn_unused_record, nowarn_unused_type | Options0];
            _ ->
                Options0
        end,
    elp_lint:module(Forms, FileName, Options, OverrideOptions).

collect_parse_transforms([], Forms, Transforms) ->
    {Transforms, lists:reverse(Forms)};
collect_parse_transforms(
    [{attribute, Line, compile, Value0} | Rest],
    Forms,
    Transforms0
) ->
    case collect_parse_transforms(Value0) of
        {drop, Transforms} ->
            collect_parse_transforms(Rest, Forms, Transforms ++ Transforms0);
        {keep, Transforms, Value} ->
            Attr = {attribute, Line, compile, Value},
            collect_parse_transforms(Rest, [Attr | Forms], Transforms ++ Transforms0)
    end;
collect_parse_transforms([Form | Rest], Forms, Transforms) ->
    collect_parse_transforms(Rest, [Form | Forms], Transforms).

collect_parse_transforms({parse_transform, Transform}) ->
    {drop, [Transform]};
%% TODO: handle lists of options
collect_parse_transforms(Other) ->
    {keep, [], Other}.

%% Replicate the way this messes up the AST without actually
%% having the transform as a dependency
transform(cth_readable_transform, Forms, _Options) ->
    cth_readable_transform(Forms);
%% MS transform can add a -compile attribute with unexpected line position,
%% we fix it up to something we can consume.
%% Setting it to {0, 0} crashes erl_lint, so we set to {0, 1}
transform(ms_transform, Forms, _Options) ->
    case ms_transform:parse_transform(Forms, []) of
        [First | Rest] ->
            case First of
                {attribute, 0, compile, Value} ->
                    [{attribute, {0, 1}, compile, Value} | Rest];
                _ ->
                    [First | Rest]
            end;
        _ -> % In case of errors or warnings, we keep the existing forms
            Forms
    end;
transform(qlc, Forms, _Options) ->
    Forms;
transform(vararg, Forms, _Options) ->
    vararg_transform(Forms);
transform(Other, Forms, _Options) ->
    Other:parse_transform(Forms, []).

cth_readable_transform({call, Line, {remote, _, {atom, _, ct}, {atom, _, pal}}, Args}) ->
    {call, Line, {remote, Line, {atom, Line, cthr}, {atom, Line, pal}}, cth_readable_transform(Args)};
cth_readable_transform(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(cth_readable_transform(tuple_to_list(Tuple)));
cth_readable_transform(List) when is_list(List) ->
    [cth_readable_transform(Elem) || Elem <- List];
cth_readable_transform(Atomic) ->
    Atomic.

vararg_transform({call, Line, {atom, Line2, 'MAKE_FUN'}, Args}) ->
    {call, Line, {remote, Line2, {atom, Line2, vararg}, {atom, Line2, 'MAKE_FUN'}}, vararg_transform(Args)};
vararg_transform(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(vararg_transform(tuple_to_list(Tuple)));
vararg_transform(List) when is_list(List) ->
    [vararg_transform(Elem) || Elem <- List];
vararg_transform(Atomic) ->
    Atomic.

-spec partition_stub([elp_parse:abstract_form()]) ->
    {Stub :: [elp_parse:abstract_form()], AST :: [elp_parse:abstract_form()], FILES :: string()}.
partition_stub(Forms) -> {[Attr || {attribute, _, _, _} = Attr <- Forms],
                          Forms,
                          % eqwalizer:ignore - We know that the Name is a string, from matching the abstract form
                          string:join([Name || {attribute, _, file, {Name,_}} = _Attr <- Forms],"\n")}.


format_errors(Forms, OriginalPath, Warnings) ->
    Formatted =
        lists:flatten([
            format_error(Forms, OriginalPath, Path, Warning)
         || {Path, FileWarnings} <- Warnings, Warning <- FileWarnings
        ]),
    term_to_binary(Formatted).

-spec format_error(
    AbsForms,
    list(),
    unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata(),
    elp_lint:error_info()
) ->
    string()
when
    AbsForms :: [erl_parse:abstract_form() | erl_parse:form_info()].
format_error(_Forms, _OriginalPath, Path, {Line, Mod, Reason}) when is_integer(Line) ->
    [
        {
            unicode:characters_to_list(Path),
            none,
            unicode:characters_to_list(
                io_lib:format("~p: ~ts", [Line, Mod:format_error(Reason)])
            ),
            erlang_service_error_codes:make_code(Mod, Reason)
        }
    ];
format_error(_Forms, SamePath, SamePath, {Location, Mod, Reason}) ->
    [
        {
            unicode:characters_to_list(SamePath),
            % Location would be {Line, Col} for a erlc compiler error/warning,
            % but {ByteStart, ByteEnd} for an eqwalizer diagnostic.
            % This is deciphered on elp side.
            Location,
            unicode:characters_to_list(
                Mod:format_error(Reason)
            ),
            erlang_service_error_codes:make_code(Mod, Reason)
        }
    ];
format_error(Forms, OriginalPath, Path, {Location, Mod, Reason}) ->
    %% The original path and reported error path are different, the
    %% error is in an included file.
    %% This can be from an include, include_lib, behaviour or parse_transform
    IncludeLocation = inclusion_range(Forms, Path),

    %% We return an error at the location the error occurs, as well as
    %% a list of the errors in the included file. ELP will determine
    %% the appropriate FileId and emit diagnostics for the include file.
    [
        {
            unicode:characters_to_list(OriginalPath),
            % Location would be {Line, Col} for a erlc compiler error/warning,
            % but {ByteStart, ByteEnd} for an eqwalizer diagnostic.
            % This is deciphered on elp side.
            IncludeLocation,
            unicode:characters_to_list("Issue in included file"),
            erlang_service_error_codes:make_code(erlang_service_error_codes, "Issue in included file")
        },
        {
            unicode:characters_to_list(Path),
            % Location would be {Line, Col} for a erlc compiler error/warning,
            % but {ByteStart, ByteEnd} for an eqwalizer diagnostic.
            % This is deciphered on elp side.
            [IncludeLocation, Location],
            unicode:characters_to_list(
                Mod:format_error(Reason)
            ),
            erlang_service_error_codes:make_code(Mod, Reason)
        }
    ].

inclusion_range(Forms, Path) ->
    case [Location || {attribute, Location, file, {FormPath, _}} <- Forms, FormPath == Path] of
        [{Loc, _}] ->
            {Loc, Loc};
        _ ->
            {1, 1}
    end.

extract_forms(FileName, Options) ->
    case filename:extension(FileName) of
        ".erl" ->
            elp_epp:parse_file(FileName, Options);
        ".hrl" ->
            elp_epp:parse_file(FileName, Options);
        ".escript" ->
            Forms = elp_escript:extract(FileName),
            {ok, Forms};
        _Ext ->
            {error, "Skipping diagnostics due to extension"}
    end.
