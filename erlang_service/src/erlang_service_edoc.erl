%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service_edoc).

-export([run/2]).

-define(DICT_KEY, edoc_diagnostics).

-type docs() ::
    #{
        module_doc := binary(),
        function_docs := [{{FunName :: atom(), FunArity :: arity()}, FunDoc :: binary()}],
        type_docs := [{{TypeName :: atom(), TypeArity :: arity()}, TypeDoc :: binary()}],
        diagnostics := [diagnostic()]
    }.
-type diagnostic() ::
    {Line :: pos_integer(), Message :: binary(), Severity :: severity()}.
-type severity() :: warning | error.
-type origin() :: eep48 | edoc.

run(_Id, [FileName, DocOrigin, AST]) ->
    Docs =
        case doc_from_eep059_attributes(FileName, DocOrigin, AST, supports_eep059_doc_attributes()) of
            {ok, D} ->
                D;
            {error, _} ->
                get_docs_for_src_file(FileName, DocOrigin)
        end,
    {ok, serialize_docs(Docs)}.

-spec supports_eep059_doc_attributes() -> boolean().
supports_eep059_doc_attributes() ->
    list_to_integer(erlang:system_info(otp_release)) >= 27.

-spec doc_from_eep059_attributes(string(), origin(), no_ast | binary(), boolean()) ->
    {ok, docs()} | {error, not_supported | skipping | no_docs}.
doc_from_eep059_attributes(_FileName, _DocOrigin, _AST, false) ->
    {error, not_supported};
doc_from_eep059_attributes(_FileName, eep48, _AST, _SupportsEEP059DocAttributes) ->
    {error, skipping};
doc_from_eep059_attributes(_FileName, _DocOrigin, no_ast, _SupportsEEP059DocAttributes) ->
    {error, skipping};
doc_from_eep059_attributes(FileName, edoc, AST0, _SupportsEEP059DocAttributes) ->
    {ok, AST, []} = binary_to_term(AST0),
    % T206726412: By default, the beam_doc module generates docs only for exported functions.
    % While we implement a native solution to handle documentation in ELP,
    % mark all functions as exported in this context.
    PatchedAST = [{attribute, 0, compile, export_all} | AST],
    case
        beam_doc:main(
            filename:dirname(FileName),
            filename:basename(FileName),
            % eqwalizer:ignore - We can guarantee that the AST is valid
            PatchedAST,
            []
        )
    of
        {ok, DocsV1, _Warnings} ->
            % elp:ignore W0023 (atoms_exhaustion)
            ModuleName = list_to_atom(filename:basename(FileName, ".erl")),
            {ok, render_docs_v1(ModuleName, DocsV1, [])};
        {error, no_docs} ->
            {error, no_docs}
    end.

-spec serialize_docs(docs()) -> [{binary(), binary()}].
serialize_docs(#{
    module_doc := ModuleDoc,
    function_docs := FunctionDocs,
    type_docs := TypeDocs,
    diagnostics := Diagnostics
}) when
    is_binary(ModuleDoc), is_list(FunctionDocs)
->
    lists:append([
        [{<<"MDC">>, ModuleDoc}],
        [serialize_function_doc(F) || F = {{_N, _A}, _D} <- FunctionDocs],
        [serialize_type_doc(T) || T = {{_N, _A}, _D} <- TypeDocs],
        [{<<"EDC">>, serialize_edoc_diagnostic(D)} || D <- lists:keysort(1, Diagnostics)]
    ]).

serialize_function_doc({{Name, Arity}, Doc}) when
    is_atom(Name), is_integer(Arity), is_binary(Doc)
->
    {<<"FDC">>,
        unicode:characters_to_binary(
            io_lib:format("~ts ~B ~ts", [Name, Arity, Doc])
        )}.

serialize_type_doc({{Name, Arity}, Doc}) when
    is_atom(Name), is_integer(Arity), is_binary(Doc)
->
    {<<"TDC">>,
        unicode:characters_to_binary(
            io_lib:format("~ts ~B ~ts", [Name, Arity, Doc])
        )}.

-spec serialize_edoc_diagnostic({Line :: pos_integer(), Message :: binary(), Severity :: severity()}) ->
    binary().
serialize_edoc_diagnostic({Line, Code, Message, Severity}) ->
    unicode:characters_to_binary(
        io_lib:format("~ts ~ts ~tp ~ts", [Code, Severity, Line, Message])
    ).

-spec get_docs_for_src_file(file:filename_all(), origin()) -> docs().
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
                        type_docs => [],
                        diagnostics => fetch_diagnostics_from_dict()
                    }
            end;
        _ ->
            #{
                module_doc => <<>>,
                function_docs => [],
                type_docs => [],
                diagnostics => []
            }
    end.

fetch_diagnostics_from_dict() ->
    [
        {Line, erlang_service_error_codes:make_code(edoc, Format),
            unicode:characters_to_binary(
                io_lib:format(Format, Args)
            ),
            Severity}
     || {Line, _Where, Format, Args, Severity} <- get(?DICT_KEY)
    ].

% Format used by OTP docs for OTP >= 27
render_docs_v1(
    _ModuleName,
    {docs_v1, _Anno, _BeamLang, <<"text/markdown">> = _Format, ModuleDoc0, _Metadata, Docs} =
        _DocsV1,
    Diagnostics = []
) ->
    ModuleDoc =
        case ModuleDoc0 of
            #{<<"en">> := En} -> En;
            _ -> <<>>
        end,
    #{
        module_doc =>
            ModuleDoc,
        function_docs =>
            % TODO: T196938446
            % This processing does not deal with `equiv` metadata, when the FDoc is empty.
            % It needs to extract the MFA from the `equiv` field and render it as a function doc.
            % Example case:
            % {{function,get_application,0},                      :: KNA
            %     {1011,1},                                       :: Anno
            %     [<<"get_application()">>],                      :: FSig
            %     none,                                           :: FDoc
            %     #{equiv => <<"get_application(self())">>}},     :: FMetadata
            % And no docs here, need to get them from {function,get_application,1}
            [
                begin
                    NA = kna_to_name_arity(KNA),
                    case FDoc of
                        #{<<"en">> := FDocEn} ->
                            FDocEn;
                        _ ->
                            case FMetadata of
                                #{equiv := Equiv} ->
                                    FDocEn = <<<<"equivalent to `">>/binary, Equiv/binary, <<"`">>/binary>>;
                                _ ->
                                    FDocEn = <<>>
                            end
                    end,
                    {NA, FDocEn}
                end
             || {{function, _N, _A} = KNA, _FAnno, _FSig, FDoc, FMetadata} <- Docs,
                none =/= kna_to_name_arity(KNA)
            ],
        type_docs =>
            [
                begin
                    NA = kna_to_name_arity(KNA),
                    case FDoc of
                        #{<<"en">> := FDocEn} ->
                            FDocEn;
                        _ ->
                            case FMetadata of
                                #{equiv := Equiv} ->
                                    FDocEn = <<<<"equivalent to `">>/binary, Equiv/binary, <<"`">>/binary>>;
                                _ ->
                                    FDocEn = <<>>
                            end
                    end,
                    {NA, FDocEn}
                end
             || {{type, _N, _A} = KNA, _FAnno, _FSig, FDoc, FMetadata} <- Docs,
                none =/= kna_to_name_arity(KNA)
            ],
        diagnostics => Diagnostics
    };
% Format used by OTP docs for OTP < 27
render_docs_v1(
    ModuleName,
    {docs_v1, _Anno, _BeamLang, <<"application/erlang+html">> = _Format, _ModuleDoc, _Metadata, FunctionDocs} =
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
        type_docs =>
            [], % No support for type docs in OTP 27
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
        type_docs => [], % No support for EDoc type docs
        diagnostics => Diagnostics
    }.

-spec kna_to_name_arity(docsh_format:kna() | none) -> {atom(), arity()} | none.
kna_to_name_arity(none) ->
    none;
kna_to_name_arity({Kind, Name, Arity}) ->
    case {Kind, Arity} of
        {function, A} when is_integer(A) ->
            {Name, Arity};
        {type, A} when is_integer(A) ->
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
