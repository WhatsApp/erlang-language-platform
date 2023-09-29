-module(erlang_service).

-export([main/1]).

-record(state, {io = erlang:group_leader()}).

-type docs() ::
    #{
        module_doc := binary(),
        function_docs := [{{FunName :: atom(), FunArity :: arity()}, FunDoc :: binary()}],
        diagnostics := [diagnostic()]
    }.
-type diagnostic() ::
    {Line :: pos_integer(), Message :: binary(), Severity :: severity()}.
-type severity() :: warning | error.

-define(DICT_KEY, edoc_diagnostics).

main(_Args) ->
    configure_logging(),
    erlang:system_flag(backtrace_depth, 20),
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
    spawn_link(fun() ->
        {Id, FileName} = binary_to_term(Data),
        try
            run_get_docs(Id, FileName, DocOrigin, State)
        catch
            Class:Reason:StackTrace ->
                Formatted = erl_error:format_exception(Class, Reason, StackTrace),
                ExceptionData = unicode:characters_to_binary(Formatted),
                reply_exception(Id, ExceptionData, State)
        end
    end),
    State.

elp_lint(BinLen, State, PostProcess, Deterministic) ->
    Len = binary_to_integer(BinLen),
    %% Use file:read/2 since it reads bytes
    {ok, Data} = file:read(State#state.io, Len),
    spawn_link(fun() ->
        {Id, FileName, Options} = binary_to_term(Data),
        try
            run_elp_lint(Id, FileName, Options, State, PostProcess, Deterministic)
        catch
            Class:Reason:StackTrace ->
                Formatted = erl_error:format_exception(Class, Reason, StackTrace),
                ExceptionData = unicode:characters_to_binary(Formatted),
                reply_exception(Id, ExceptionData, State)
        end
    end),
    State.

run_get_docs(Id, FileName, DocOrigin, State) ->
    Result = serialize_docs(get_docs_for_src_file(FileName, DocOrigin)),
    reply(Id, Result, State).

run_elp_lint(Id, FileName, Options0, State, PostProcess, Deterministic) ->
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

    case MaybeForms of
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
            case lint_file(Forms3, FileName, Options3) of
                {ok, []} ->
                    {Stub, AST} = partition_stub(Forms3),
                    ResultStub = PostProcess(Stub, FileName),
                    ResultAST = PostProcess(AST, FileName),
                    reply(Id, [{"AST", ResultAST}, {"STUB", ResultStub}], State);
                {ok, Warnings} ->
                    {Stub, AST} = partition_stub(Forms3),
                    ResultStub = PostProcess(Stub, FileName),
                    ResultAST = PostProcess(AST, FileName),
                    FormattedWarnings = format_errors(Forms3, FileName, Warnings),
                    reply(Id, [{"AST", ResultAST}, {"STUB", ResultStub}, {"WARNINGS", FormattedWarnings}], State);
                {error, Errors, Warnings} ->
                    {Stub, AST} = partition_stub(Forms3),
                    ResultStub = PostProcess(Stub, FileName),
                    ResultAST = PostProcess(AST, FileName),
                    FormattedErrors = format_errors(Forms3, FileName, Errors),
                    FormattedWarnings = format_errors(Forms3, FileName, Warnings),
                    reply(
                        Id,
                        [
                            {"AST", ResultAST},
                            {"STUB", ResultStub},
                            {"ERRORS", FormattedErrors},
                            {"WARNINGS", FormattedWarnings}
                        ],
                        State
                    )
            end;
        {error, Reason} ->
            Msg = unicode:characters_to_binary(
                file:format_error(Reason)
            ),
            reply_exception(Id, Msg, State)
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

-spec get_docs_for_src_file(_FileName, edoc | eep48) -> docs().
get_docs_for_src_file(FileName, Origin) ->
    put(?DICT_KEY, []),
    case filename:extension(FileName) of
        ".erl" ->
            ModuleName = list_to_atom(filename:basename(FileName, ".erl")),
            try
                Docs =
                    case Origin of
                        eep48 ->
                            case code:get_doc(ModuleName) of
                                {ok, DocV1} ->
                                    DocV1;
                                {error, Reason} ->
                                    throw(
                                        lists:flatten(
                                            io_lib:format(
                                                "Failed to load docs via compiled beam for source file ~ts: "
                                                "~ts",
                                                [FileName, Reason]
                                            )
                                        )
                                    )
                            end;
                        edoc ->
                            {_, EDoc} = edoc:get_doc(FileName, [
                                {private, true}, {preprocess, false}
                            ]),
                            % docsh_edoc_xmerl is sub-optimal because it doesn't generate metadata, etc. - just the core textual documentation -
                            % but it's all we have in terms of something that can turn edoc xml into plain text or markdown
                            Internal = xmerl:export_simple([EDoc], docsh_edoc_xmerl),
                            docsh_docs_v1:from_internal(Internal)
                    end,
                render_docs_v1(ModuleName, Docs, fetch_diagnostics_from_dict())
            catch
                _:_E:_Trace ->
                    #{
                        module_doc => <<>>,
                        function_docs => [],
                        diagnostics => fetch_diagnostics_from_dict()
                    }
            end;
        _ ->
            #{
                module_doc => <<>>,
                function_docs => [],
                diagnostics => []
            }
    end.

fetch_diagnostics_from_dict() ->
    [
        {Line, make_code(edoc, Format),
            unicode:characters_to_binary(
                io_lib:format(Format, Args)
            ),
            Severity}
     || {Line, _Where, Format, Args, Severity} <- get(?DICT_KEY)
    ].

% Format used by OTP docs
render_docs_v1(
    ModuleName,
    {docs_v1, _Anno, _BeamLang, <<"application/erlang+html">> = _Format, _ModuleDoc, _Metadata,
        FunctionDocs} =
        DocsV1,
    Diagnostics = []
) ->
    % Render using a standard, simple format - same as `h(ModuleName)` in the `erl` shell
    % See https://www.erlang.org/doc/man/shell_docs.html#description
    #{
        module_doc =>
            unicode:characters_to_binary(
                render_eep48_docs:render(ModuleName, DocsV1)
            ),
        function_docs =>
            [
                begin
                    {FName, FArity} = NA = kna_to_name_arity(KNA),
                    FDoc =
                        case render_eep48_docs:render(ModuleName, FName, FArity, DocsV1) of
                            {error, function_missing} ->
                                <<>>;
                            Res ->
                                unicode:characters_to_binary(Res)
                        end,
                    {NA, FDoc}
                end
             || {KNA, _FAnno, _FSig, _FDoc, _FMetadata} <- FunctionDocs,
                none =/= kna_to_name_arity(KNA)
            ],
        diagnostics => Diagnostics
    };
% Format used by edoc
render_docs_v1(
    _ModuleName,
    {docs_v1, _Anno, _BeamLang, <<"text/erlang-edoc">> = _Format, ModuleDoc, _Metadata, ItemDocs},
    Diagnostics
) ->
    ModuleDocEn =
        case ModuleDoc of
            _ when is_map(ModuleDoc) ->
                render_as_markdown(maps:get(<<"en">>, ModuleDoc, <<>>));
            _ ->
                <<>>
        end,
    FunctionDocs = [item_doc(Item) || Item <- ItemDocs],
    #{
        module_doc => ModuleDocEn,
        function_docs => [{F, FDoc} || {{_FName, _FArity} = F, FDoc} <- FunctionDocs],
        diagnostics => Diagnostics
    }.

-spec kna_to_name_arity(docsh_format:kna() | none) -> {atom(), arity()} | none.
kna_to_name_arity(none) ->
    none;
kna_to_name_arity({Kind, Name, Arity}) ->
    case {Kind, Arity} of
        {function, A} when is_integer(A) ->
            % TODO Support type docs?
            {Name, Arity};
        _ ->
            none
    end.

render_as_markdown(none) ->
    none;
render_as_markdown(Doc) ->
    StructuredDoc = docsh_edoc_xmerl:format_edoc(Doc, #{}),
    doc_to_md(StructuredDoc).

doc_to_md(Components) ->
    FormattedEntries = [form_markdown(C) || C <- Components],
    unicode:characters_to_binary(
        string:join(FormattedEntries, "\n\n")
    ).

-spec form_markdown(tuple()) -> string().
form_markdown({h1, String}) ->
    "# " ++ String;
form_markdown({h2, String}) ->
    "## " ++ String;
form_markdown({h3, String}) ->
    "### " ++ String;
form_markdown({h4, String}) ->
    "#### " ++ String;
form_markdown({code_block_line, String}) ->
    "  " ++ String;
form_markdown({code_block_begin, Language}) ->
    "```" ++ Language;
form_markdown({code_block_end, _Language}) ->
    "```";
form_markdown({code_line, String}) ->
    "`" ++ String ++ "`";
form_markdown({_Form, String}) ->
    String;
form_markdown(Binary) when is_binary(Binary) ->
    Binary;
form_markdown(String) when is_list(String) ->
    String.

-spec item_doc(Item :: dochsh_docs_v1:item()) -> {{atom(), arity()}, binary()} | none.
item_doc({KNA, _Anno, _Sig, Doc, _Metadata}) ->
    case Doc of
        none ->
            none;
        hidden ->
            none;
        #{<<"en">> := ItemDoc} ->
            case kna_to_name_arity(KNA) of
                none ->
                    none;
                {_Name, _Arity} = NameArity ->
                    {NameArity, render_as_markdown(ItemDoc)}
            end;
        _ ->
            none
    end;
item_doc(_) ->
    none.

-spec serialize_docs(docs()) -> [{string(), binary()}].
serialize_docs(#{
    module_doc := ModuleDoc,
    function_docs := FunctionDocs,
    diagnostics := Diagnostics
}) when
    is_binary(ModuleDoc), is_list(FunctionDocs)
->
    lists:append([
        [{"MODULE_DOC", ModuleDoc}],
        [serialize_function_doc(F) || F = {{_N, _A}, _D} <- FunctionDocs],
        [{"EDOC_DIAGNOSTIC", serialize_edoc_diagnostic(D)} || D <- lists:keysort(1, Diagnostics)]
    ]).

serialize_function_doc({{Name, Arity}, Doc}) when
    is_atom(Name), is_integer(Arity), is_binary(Doc)
->
    {"FUNCTION_DOC",
        unicode:characters_to_binary(
            io_lib:format("~ts ~B ~ts", [Name, Arity, Doc])
        )}.

-spec serialize_edoc_diagnostic({Line :: pos_integer(), Message :: binary(), Severity :: severity()}) ->
    binary().
serialize_edoc_diagnostic({Line, Code, Message, Severity}) ->
    unicode:characters_to_binary(
        io_lib:format("~ts ~ts ~tp ~ts", [Code, Severity, Line, Message])
    ).

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
            make_code(Mod, Reason)
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
            make_code(Mod, Reason)
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
            make_code(?MODULE, "Issue in included file")
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
            make_code(Mod, Reason)
        }
    ].

inclusion_range(Forms, Path) ->
    case [Location || {attribute, Location, file, {FormPath, _}} <- Forms, FormPath == Path] of
        [{Loc, _}] ->
            {Loc, Loc};
        _ ->
            {1, 1}
    end.

reply_exception(Id, Data, State) ->
    %% Use file:write/2 since it writes bytes
    Size = integer_to_binary(byte_size(Data)),
    BinId = integer_to_binary(Id),
    file:write(State#state.io, [<<"EXCEPTION ">>, BinId, $\s, Size, $\n | Data]),
    ok.

reply(Id, Segments, State) ->
    %% Use file:write/2 since it writes bytes
    BinId = integer_to_binary(Id),
    Size = integer_to_binary(length(Segments)),
    Data = [encode_segment(Segment) || Segment <- Segments],
    file:write(State#state.io, [<<"REPLY ">>, BinId, $\s, Size, $\n | Data]),
    ok.

encode_segment({Tag, Data}) ->
    Size = integer_to_binary(byte_size(Data)),
    [Tag, $\s, Size, $\n | Data].

-spec partition_stub([elp_parse:abstract_form()]) ->
    {Stub :: [elp_parse:abstract_form()], AST :: [elp_parse:abstract_form()]}.
partition_stub(Forms) -> {[Attr || {attribute, _, _, _} = Attr <- Forms], Forms}.

%-----------------------------------------------------------------------

%% The make_code/2 function is based on the one in erlang_ls

%% @doc Return a unique code for each of the possible errors returned by the
%% compiler or its subsystems.

-spec make_code(atom(), any()) -> string().
%% This file
make_code(?MODULE, _) ->
    "L0000";
%% compiler-8.0.2/src/compile.erl
make_code(compile, no_crypto) ->
    "C1000";
make_code(compile, bad_crypto_key) ->
    "C1001";
make_code(compile, no_crypto_key) ->
    "C1002";
make_code(compile, {open, _E}) ->
    "C1003";
make_code(compile, {epp, _E}) ->
    make_code(epp, compile);
make_code(compile, write_error) ->
    "C1004";
make_code(compile, {write_error, _Error}) ->
    "C1005";
make_code(compile, {rename, _From, _To, _Error}) ->
    "C1006";
make_code(compile, {parse_transform, _M, _R}) ->
    "C1007";
make_code(compile, {undef_parse_transform, _M}) ->
    "C1008";
make_code(compile, {core_transform, _M, _R}) ->
    "C1009";
make_code(compile, {crash, _Pass, _Reason, _Stk}) ->
    "C1010";
make_code(compile, {bad_return, _Pass, _Reason}) ->
    "C1011";
make_code(compile, {module_name, _Mod, _Filename}) ->
    "C1012";
make_code(compile, _Other) ->
    "C1099";
%% syntax_tools-2.6/src/epp_dodger.erl
make_code(epp_dodger, macro_args) ->
    "D1100";
make_code(epp_dodger, {error, _Error}) ->
    "D1101";
make_code(epp_dodger, {warning, _Error}) ->
    "D1102";
make_code(epp_dodger, {unknown, _Reason}) ->
    "D1103";
make_code(epp_dodger, _Other) ->
    "D1199";
%% stdlib-3.15.2/src/erl_lint.erl
make_code(elp_lint, undefined_module) ->
    "L1201";
make_code(elp_lint, redefine_module) ->
    "L1202";
make_code(elp_lint, pmod_unsupported) ->
    "L1203";
make_code(elp_lint, non_latin1_module_unsupported) ->
    "L1204";
make_code(elp_lint, invalid_call) ->
    "L1205";
make_code(elp_lint, invalid_record) ->
    "L1206";
make_code(elp_lint, {attribute, _A}) ->
    "L1207";
make_code(elp_lint, {missing_qlc_hrl, _A}) ->
    "L1208";
make_code(elp_lint, {redefine_import, {{_F, _A}, _M}}) ->
    "L1209";
make_code(elp_lint, {bad_inline, {_F, _A}}) ->
    "L1210";
make_code(elp_lint, {invalid_deprecated, _D}) ->
    "L1211";
make_code(elp_lint, {bad_deprecated, {_F, _A}}) ->
    "L1212";
make_code(elp_lint, {invalid_removed, _D}) ->
    "L1213";
make_code(elp_lint, {bad_removed, {F, A}}) when F =:= '_'; A =:= '_' ->
    "L1214";
make_code(elp_lint, {bad_removed, {_F, _A}}) ->
    "L1215";
make_code(elp_lint, {bad_nowarn_unused_function, {_F, _A}}) ->
    "L1216";
make_code(elp_lint, {bad_nowarn_bif_clash, {_F, _A}}) ->
    "L1217";
make_code(elp_lint, disallowed_nowarn_bif_clash) ->
    "L1218";
make_code(elp_lint, {bad_on_load, _Term}) ->
    "L1219";
make_code(elp_lint, multiple_on_loads) ->
    "L1220";
make_code(elp_lint, {bad_on_load_arity, {_F, _A}}) ->
    "L1221";
make_code(elp_lint, {undefined_on_load, {_F, _A}}) ->
    "L1222";
make_code(elp_lint, nif_inline) ->
    "L1223";
make_code(elp_lint, export_all) ->
    "L1224";
make_code(elp_lint, {duplicated_export, {_F, _A}}) ->
    "L1225";
make_code(elp_lint, {unused_import, {{_F, _A}, _M}}) ->
    "L1226";
make_code(elp_lint, {undefined_function, {_F, _A}}) ->
    "L1227";
make_code(elp_lint, {redefine_function, {_F, _A}}) ->
    "L1228";
make_code(elp_lint, {define_import, {_F, _A}}) ->
    "L1229";
make_code(elp_lint, {unused_function, {_F, _A}}) ->
    "L1230";
make_code(elp_lint, {call_to_redefined_bif, {_F, _A}}) ->
    "L1231";
make_code(elp_lint, {call_to_redefined_old_bif, {_F, _A}}) ->
    "L1232";
make_code(elp_lint, {redefine_old_bif_import, {_F, _A}}) ->
    "L1233";
make_code(elp_lint, {redefine_bif_import, {_F, _A}}) ->
    "L1234";
make_code(elp_lint, {deprecated, _MFA, _String, _Rel}) ->
    "L1235";
make_code(elp_lint, {deprecated, _MFA, String}) when is_list(String) ->
    "L1236";
make_code(elp_lint, {deprecated_type, {_M1, _F1, _A1}, _String, _Rel}) ->
    "L1237";
make_code(elp_lint, {deprecated_type, {_M1, _F1, _A1}, String}) when is_list(String) ->
    "L1238";
make_code(elp_lint, {removed, _MFA, _ReplacementMFA, _Rel}) ->
    "L1239";
make_code(elp_lint, {removed, _MFA, String}) when is_list(String) ->
    "L1240";
make_code(elp_lint, {removed_type, _MNA, _String}) ->
    "L1241";
make_code(elp_lint, {obsolete_guard, {_F, _A}}) ->
    "L1242";
make_code(elp_lint, {obsolete_guard_overridden, _Test}) ->
    "L1243";
make_code(elp_lint, {too_many_arguments, _Arity}) ->
    "L1244";
make_code(elp_lint, illegal_pattern) ->
    "L1245";
make_code(elp_lint, illegal_map_key) ->
    "L1246";
make_code(elp_lint, illegal_bin_pattern) ->
    "L1247";
make_code(elp_lint, illegal_expr) ->
    "L1248";
make_code(elp_lint, {illegal_guard_local_call, {_F, _A}}) ->
    "L1249";
make_code(elp_lint, illegal_guard_expr) ->
    "L1250";
make_code(elp_lint, illegal_map_construction) ->
    "L1251";
make_code(elp_lint, {undefined_record, _T}) ->
    "L1252";
make_code(elp_lint, {redefine_record, _T}) ->
    "L1253";
make_code(elp_lint, {redefine_field, _T, _F}) ->
    "L1254";
make_code(elp_lint, bad_multi_field_init) ->
    "L1255";
make_code(elp_lint, {undefined_field, _T, _F}) ->
    "L1256";
make_code(elp_lint, illegal_record_info) ->
    "L1257";
make_code(elp_lint, {field_name_is_variable, _T, _F}) ->
    "L1258";
make_code(elp_lint, {wildcard_in_update, _T}) ->
    "L1259";
make_code(elp_lint, {unused_record, _T}) ->
    "L1260";
make_code(elp_lint, {untyped_record, _T}) ->
    "L1261";
make_code(elp_lint, {unbound_var, _V}) ->
    "L1262";
make_code(elp_lint, {unsafe_var, _V, {_What, _Where}}) ->
    "L1263";
make_code(elp_lint, {exported_var, _V, {_What, _Where}}) ->
    "L1264";
make_code(elp_lint, {match_underscore_var, _V}) ->
    "L1265";
make_code(elp_lint, {match_underscore_var_pat, _V}) ->
    "L1266";
make_code(elp_lint, {shadowed_var, _V, _In}) ->
    "L1267";
make_code(elp_lint, {unused_var, _V}) ->
    "L1268";
make_code(elp_lint, {variable_in_record_def, _V}) ->
    "L1269";
make_code(elp_lint, {stacktrace_guard, _V}) ->
    "L1270";
make_code(elp_lint, {stacktrace_bound, _V}) ->
    "L1271";
make_code(elp_lint, {undefined_bittype, _Type}) ->
    "L1272";
make_code(elp_lint, {bittype_mismatch, _Val1, _Val2, _What}) ->
    "L1273";
make_code(elp_lint, bittype_unit) ->
    "L1274";
make_code(elp_lint, illegal_bitsize) ->
    "L1275";
make_code(elp_lint, {illegal_bitsize_local_call, {_F, _A}}) ->
    "L1276";
make_code(elp_lint, non_integer_bitsize) ->
    "L1277";
make_code(elp_lint, unsized_binary_not_at_end) ->
    "L1278";
make_code(elp_lint, typed_literal_string) ->
    "L1279";
make_code(elp_lint, utf_bittype_size_or_unit) ->
    "L1280";
make_code(elp_lint, {bad_bitsize, _Type}) ->
    "L1281";
make_code(elp_lint, unsized_binary_in_bin_gen_pattern) ->
    "L1282";
make_code(elp_lint, {conflicting_behaviours, {_Name, _Arity}, _B, _FirstL, _FirstB}) ->
    "L1283";
make_code(elp_lint, {undefined_behaviour_func, {_Func, _Arity}, _Behaviour}) ->
    "L1284";
make_code(elp_lint, {undefined_behaviour, _Behaviour}) ->
    "L1285";
make_code(elp_lint, {undefined_behaviour_callbacks, _Behaviour}) ->
    "L1286";
make_code(elp_lint, {ill_defined_behaviour_callbacks, _Behaviour}) ->
    "L1287";
make_code(elp_lint, {ill_defined_optional_callbacks, _Behaviour}) ->
    "L1288";
make_code(elp_lint, {behaviour_info, {_M, _F, _A}}) ->
    "L1289";
make_code(elp_lint, {redefine_optional_callback, {_F, _A}}) ->
    "L1290";
make_code(elp_lint, {undefined_callback, {_M, _F, _A}}) ->
    "L1291";
make_code(elp_lint, {singleton_typevar, _Name}) ->
    "L1292";
make_code(elp_lint, {bad_export_type, _ETs}) ->
    "L1293";
make_code(elp_lint, {duplicated_export_type, {_T, _A}}) ->
    "L1294";
make_code(elp_lint, {undefined_type, {_TypeName, _Arity}}) ->
    "L1295";
make_code(elp_lint, {unused_type, {_TypeName, _Arity}}) ->
    "L1296";
make_code(elp_lint, {new_builtin_type, {_TypeName, _Arity}}) ->
    "L1297";
make_code(elp_lint, {builtin_type, {_TypeName, _Arity}}) ->
    "L1298";
make_code(elp_lint, {renamed_type, _OldName, _NewName}) ->
    "L1299";
make_code(elp_lint, {redefine_type, {_TypeName, _Arity}}) ->
    "L1300";
make_code(elp_lint, {type_syntax, _Constr}) ->
    "L1301";
make_code(elp_lint, old_abstract_code) ->
    "L1302";
make_code(elp_lint, {redefine_spec, {_M, _F, _A}}) ->
    "L1303";
make_code(elp_lint, {redefine_spec, {_F, _A}}) ->
    "L1304";
make_code(elp_lint, {redefine_callback, {_F, _A}}) ->
    "L1305";
make_code(elp_lint, {bad_callback, {_M, _F, _A}}) ->
    "L1306";
make_code(elp_lint, {bad_module, {_M, _F, _A}}) ->
    "L1307";
make_code(elp_lint, {spec_fun_undefined, {_F, _A}}) ->
    "L1308";
make_code(elp_lint, {missing_spec, {_F, _A}}) ->
    "L1309";
make_code(elp_lint, spec_wrong_arity) ->
    "L1310";
make_code(elp_lint, callback_wrong_arity) ->
    "L1311";
make_code(elp_lint, {deprecated_builtin_type, {_Name, _Arity}, _Replacement, _Rel}) ->
    "L1312";
make_code(elp_lint, {not_exported_opaque, {_TypeName, _Arity}}) ->
    "L1313";
make_code(elp_lint, {underspecified_opaque, {_TypeName, _Arity}}) ->
    "L1314";
make_code(elp_lint, {bad_dialyzer_attribute, _Term}) ->
    "L1315";
make_code(elp_lint, {bad_dialyzer_option, _Term}) ->
    "L1316";
make_code(elp_lint, {format_error, {_Fmt, _Args}}) ->
    "L1317";
make_code(elp_lint, _Other) ->
    "L1399";
%% stdlib-3.15.2/src/erl_scan.erl
make_code(elp_scan, {string, _Quote, _Head}) ->
    "S1400";
make_code(elp_scan, {illegal, _Type}) ->
    "S1401";
make_code(elp_scan, char) ->
    "S1402";
make_code(elp_scan, {base, _Base}) ->
    "S1403";
make_code(elp_scan, _Other) ->
    "S1499";
%% stdlib-3.15.2/src/epp.erl
make_code(elp_epp, cannot_parse) ->
    "E1500";
make_code(elp_epp, {bad, _W}) ->
    "E1501";
make_code(elp_epp, {duplicated_argument, _Arg}) ->
    "E1502";
make_code(elp_epp, missing_parenthesis) ->
    "E1503";
make_code(elp_epp, missing_comma) ->
    "E1504";
make_code(elp_epp, premature_end) ->
    "E1505";
make_code(elp_epp, {call, _What}) ->
    "E1506";
make_code(elp_epp, {undefined, _M, none}) ->
    "E1507";
make_code(elp_epp, {undefined, _M, _A}) ->
    "E1508";
make_code(elp_epp, {depth, _What}) ->
    "E1509";
make_code(elp_epp, {mismatch, _M}) ->
    "E1510";
make_code(elp_epp, {arg_error, _M}) ->
    "E1511";
make_code(elp_epp, {redefine, _M}) ->
    "E1512";
make_code(elp_epp, {redefine_predef, _M}) ->
    "E1513";
make_code(elp_epp, {circular, _M, none}) ->
    "E1514";
make_code(elp_epp, {circular, _M, _A}) ->
    "E1515";
make_code(elp_epp, {include, _W, _F}) ->
    "E1516";
make_code(elp_epp, {illegal, _How, _What}) ->
    "E1517";
make_code(elp_epp, {illegal_function, _Macro}) ->
    "E1518";
make_code(elp_epp, {illegal_function_usage, _Macro}) ->
    "E1519";
make_code(elp_epp, elif_after_else) ->
    "E1520";
make_code(elp_epp, {'NYI', _What}) ->
    "E1521";
make_code(elp_epp, {error, _Term}) ->
    "E1522";
make_code(elp_epp, {warning, _Term}) ->
    "E1523";
make_code(elp_epp, _E) ->
    "E1599";
%% stdlib-3.15.2/src/qlc.erl
make_code(qlc, not_a_query_list_comprehension) ->
    "Q1600";
make_code(qlc, {used_generator_variable, _V}) ->
    "Q1601";
make_code(qlc, binary_generator) ->
    "Q1602";
make_code(qlc, too_complex_join) ->
    "Q1603";
make_code(qlc, too_many_joins) ->
    "Q1604";
make_code(qlc, nomatch_pattern) ->
    "Q1605";
make_code(qlc, nomatch_filter) ->
    "Q1606";
make_code(qlc, {Location, _Mod, _Reason}) when is_integer(Location) ->
    "Q1607";
make_code(qlc, {bad_object, _FileName}) ->
    "Q1608";
make_code(qlc, bad_object) ->
    "Q1609";
make_code(qlc, {file_error, _FileName, _Reason}) ->
    "Q1610";
make_code(qlc, {premature_eof, _FileName}) ->
    "Q1611";
make_code(qlc, {tmpdir_usage, _Why}) ->
    "Q1612";
make_code(qlc, {error, _Module, _Reason}) ->
    "Q1613";
make_code(qlc, _E) ->
    "Q1699";
%% stdlib-3.15.2/src/erl_parse.yrl
make_code(elp_parse, "head mismatch") ->
    "P1700";
make_code(elp_parse, "bad type variable") ->
    "P1701";
make_code(elp_parse, "bad attribute") ->
    "P1702";
make_code(elp_parse, "unsupported constraint" ++ _) ->
    "P1703";
make_code(elp_parse, "bad binary type") ->
    "P1704";
make_code(elp_parse, "bad variable list") ->
    "P1705";
make_code(elp_parse, "bad function arity") ->
    "P1706";
make_code(elp_parse, "bad function name") ->
    "P1707";
make_code(elp_parse, "bad Name/Arity") ->
    "P1708";
make_code(elp_parse, "bad record declaration") ->
    "P1709";
make_code(elp_parse, "bad record field") ->
    "P1710";
make_code(elp_parse, ["syntax error before: ", _]) ->
    "P1711";
%% Matching 'io_lib:format("bad ~tw declaration", [S])).', must come last
make_code(elp_parse, "bad " ++ _Str) ->
    "P1798";
make_code(elp_parse, _Other) ->
    "P1799";
%% Erlang EDoc
make_code(edoc, "XML parse error: ~p.") ->
    "O0001";
make_code(edoc, "error in XML parser: ~P.") ->
    "O0002";
make_code(edoc, "nocatch in XML parser: ~P.") ->
    "O0003";
make_code(edoc, "heading end marker mismatch: ~s...~s") ->
    "O0004";
make_code(edoc, "`-quote ended unexpectedly at line ~w") ->
    "O0005";
make_code(edoc, "``-quote ended unexpectedly at line ~w") ->
    "O0006";
make_code(edoc, "```-quote ended unexpectedly at line ~w") ->
    "O0007";
make_code(edoc, "reference '[~ts:...' ended unexpectedly") ->
    "O0008";
make_code(edoc, "cannot handle guard") ->
    "O0009";
make_code(edoc, "error reading file '~ts': ~w") ->
    "O0010";
make_code(edoc, "file not found: ~ts") ->
    "O0011";
make_code(edoc, "expected file name as a string") ->
    "O0012";
make_code(edoc, "@spec arity does not match.") ->
    "O0013";
make_code(edoc, "@spec name does not match.") ->
    "O0014";
make_code(edoc, "must specify name or e-mail.") ->
    "O0015";
make_code(edoc, "redefining built-in type '~w'.") ->
    "O0016";
make_code(edoc, "multiple '<...>' sections.") ->
    "O0017";
make_code(edoc, "multiple '[...]' sections.") ->
    "O0018";
make_code(edoc, "missing '~c'.") ->
    "O0019";
make_code(edoc, "unexpected end of expression.") ->
    "O0020";
make_code(edoc, "multiple @~s tag.") ->
    "O0021";
make_code(edoc, "tag @~s not allowed here.") ->
    "O0022";
make_code(edoc, "bad macro definition: ~P.") ->
    "O0023";
make_code(edoc, "cannot find application directory for '~s'.") ->
    "O0024";
make_code(edoc, "recursive macro expansion of {@~s}.") ->
    "O0025";
make_code(edoc, "undefined macro {@~s}.") ->
    "O0026";
make_code(edoc, "unexpected end of macro.") ->
    "O0027";
make_code(edoc, "missing macro name.") ->
    "O0028";
make_code(edoc, "bad macro name: '@~s...'.") ->
    "O0029";
make_code(edoc, "reference to untyped record ~w") ->
    "O0030";
make_code(edoc, "'~s' is not allowed - skipping tag, extracting content") ->
    "O0031";
make_code(edoc,
    "cannot handle spec with constraints - arity mismatch.\n"
    "This is a bug in EDoc spec formatter - please report it at "
    "https://bugs.erlang.org/\n"
    "Identified arguments: ~p\n"
    "Original spec: ~s\n"
) ->
    "O0032";
make_code(edoc,
    "cannot annotate spec: "
    "function and spec clause numbers do not match\n"
) ->
    "O0033";
make_code(edoc,
    "EDoc @spec tags are deprecated. "
    "Please use -spec attributes instead."
) ->
    "O0034";
make_code(edoc,
    "EDoc @type tags are deprecated. "
    "Please use -type attributes instead."
) ->
    "O0035";
make_code(edoc, "redefining built-in type '~w'.") ->
    "O0036";
make_code(edoc, "duplicated type ~w~s") ->
    "O0037";
make_code(edoc, "missing type ~w~s") ->
    "O0038";
make_code(edoc, "tag @~s not recognized.") ->
    "O0039";
make_code(edoc, _NoCategory) ->
    "O0000";
make_code(Module, _Reason) ->
    unicode:characters_to_list(
        io_lib:format("~p", [Module])
    ).

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
