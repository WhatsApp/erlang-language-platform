-module(erlang_service).

-export([main/1,
    run_elp_lint/4
]).

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

run_elp_lint(FileName, Options0, PostProcess, Deterministic) ->
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
    Module = epp_module(Options2),
    %% TODO workaround to enable parsing third-party deps
    %% remove it after merge of https://github.com/jlouis/graphql-erlang/pull/225
    Options3 =
        case filename:basename(FileName) of
            "graphql_execute.erl" ->
                [{no_auto_import, [{alias, 1}]} | Options2];
            _ ->
                Options2
        end,
    MaybeForms =
        case filename:extension(FileName) of
            ".erl" ->
                Module:parse_file(FileName, Options3);
            ".hrl" ->
                Module:parse_file(FileName, Options3);
            ".escript" ->
                Forms = elp_escript:extract(Module, FileName),
                {ok, Forms};
            _Ext ->
                {error, "Skipping diagnostics due to extension"}
        end,

    {ok, Forms0} = MaybeForms,
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
    case lint_file(Forms3, FileName, Options3) of
        {ok, []} ->
            {Stub, AST} = partition_stub(Forms3),
            ResultStub = PostProcess(Stub, FileName),
            ResultAST = PostProcess(AST, FileName),
            [{"AST", ResultAST}, {"STUB", ResultStub}];
        {ok, Warnings} ->
            {Stub, AST} = partition_stub(Forms3),
            ResultStub = PostProcess(Stub, FileName),
            ResultAST = PostProcess(AST, FileName),
            FormattedWarnings = format_errors(Forms3, FileName, Warnings),
            [{"AST", ResultAST}, {"STUB", ResultStub}, {"WARNINGS", FormattedWarnings}];
        {error, Errors, Warnings} ->
            {Stub, AST} = partition_stub(Forms3),
            ResultStub = PostProcess(Stub, FileName),
            ResultAST = PostProcess(AST, FileName),
            FormattedErrors = format_errors(Forms3, FileName, Errors),
            FormattedWarnings = format_errors(Forms3, FileName, Warnings),
            [{"AST", ResultAST},
             {"STUB", ResultStub},
             {"ERRORS", FormattedErrors},
             {"WARNINGS", FormattedWarnings}
            ]
    end.

lint_file(Forms, FileName, Options0) ->
    Options = case filename:extension(FileName) of
        ".hrl" ->
            [nowarn_unused_function, nowarn_unused_record, nowarn_unused_type|Options0];
        _ ->
            Options0
    end,
    elp_lint:module(Forms, FileName, Options).

epp_module(Options) ->
    case proplists:get_value(location, Options) of
        offset ->
            elp_epp;
        _ ->
            epp
    end.

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
transform(ms_transform, Forms, Options) ->
    Loc = proplists:get_value(location, Options),
    [First | Rest] = ms_transform:parse_transform(Forms, []),
    case First of
        {attribute, 0, compile, Value} when Loc =:= offset ->
            [{attribute, {0, 1}, compile, Value} | Rest];
        _ ->
            [First | Rest]
    end;
transform(qlc, Forms, _Options) ->
    Forms;
transform(vararg, Forms, _Options) ->
    vararg_transform(Forms);
transform(Other, Forms, _Options) ->
    Other:parse_transform(Forms, []).

cth_readable_transform({call, Line, {remote, _, {atom, _, ct}, {atom, _, pal}}, Args}) ->
    {call, Line, {remote, Line, {atom, Line, cthr}, {atom, Line, pal}},
        cth_readable_transform(Args)};
cth_readable_transform(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(cth_readable_transform(tuple_to_list(Tuple)));
cth_readable_transform(List) when is_list(List) ->
    [cth_readable_transform(Elem) || Elem <- List];
cth_readable_transform(Atomic) ->
    Atomic.

vararg_transform({call, Line, {atom, Line2, 'MAKE_FUN'}, Args}) ->
    {call, Line, {remote, Line2, {atom, Line2, vararg}, {atom, Line2, 'MAKE_FUN'}},
        vararg_transform(Args)};
vararg_transform(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(vararg_transform(tuple_to_list(Tuple)));
vararg_transform(List) when is_list(List) ->
    [vararg_transform(Elem) || Elem <- List];
vararg_transform(Atomic) ->
    Atomic.

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

-spec partition_stub([elp_parse:abstract_form()]) ->
    {Stub :: [elp_parse:abstract_form()], AST :: [elp_parse:abstract_form()]}.
partition_stub(Forms) -> {[Attr || {attribute, _, _, _} = Attr <- Forms], Forms}.

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
