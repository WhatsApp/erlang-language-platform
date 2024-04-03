%% % @format
-module(wa_build_info_prv).
-oncall("whatsapp_elp").

-export([init/1, do/1]).

-define(NUM_RETRIES_FETCH_DEPS, 5).

-type app_build_opts() :: #{
    name := binary(),
    dir := binary(),
    ebin := binary(),
    src_dirs := [binary()],
    extra_src_dirs := [binary()],
    include_dirs := [binary()],
    macros := [atom() | {atom(), any()}],
    parse_transforms := [any()]
}.

init(State) ->
    State1 = rebar_state:add_provider(
        State,
        providers:create([
            {name, build_info},
            {module, wa_build_info_prv},
            {bare, true},
            {deps, [app_discovery]},
            {example, "rebar3 build_info"},
            {short_desc, "Get build_info"},
            {desc, "Get build_info"},
            {opts, [
                {to, $t, "to", {string, undefined},
                    "file to write buid_info in file (ETF for LAP, or erlang_ls.config format)"},
                {mode, $m, "mode", {string, "etf"},
                    "build info format, can be etf (generic build info for ELP and Eqwalizer), els (Erlang LS), rl (Readme lint), wm (wiki_moduledocs lint)"},
                {els_config, $e, "els-config", {string, undefined}, "existing erlang_ls.config"}
            ]}
        ])
    ),
    {ok, State1}.

do(State0) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State0),
    Mode = proplists:get_value(mode, RawOpts),
    State1 = maybe_get_deps(State0, Mode),
    case Mode of
        "etf" -> etf_build_info(State1, RawOpts);
        "els" -> els_build_info(State1, RawOpts);
        "rl" -> rl_build_info(State1, RawOpts);
        "wm" -> wm_build_info(State1, RawOpts)
    end,
    {ok, State1}.

maybe_get_deps(State0, Mode) ->
    %% fetching dependencies in CI may require multiple attemps due to
    %% some unreliable source constrol infra
    case Mode of
        %% Dependencies not needed for "rl" or "wm" modes
        Mode when Mode == "rl"; Mode == "wm" ->
            State0;
        _ ->
            State1 = safe_get_install_deps(State0, ?NUM_RETRIES_FETCH_DEPS),
            {ok, State2} = rebar_prv_lock:do(State1),
            State2
    end.

safe_get_install_deps(_State, 0) ->
    rebar_log:log(error, "Unable to fetch dependencies after ~b tries, aborting", [
        ?NUM_RETRIES_FETCH_DEPS
    ]),
    throw(rebar_abort);
safe_get_install_deps(State, N) ->
    try rebar_prv_install_deps:do(State) of
        {ok, NewState} -> NewState
    catch
        E:R:ST ->
            rebar_log:log(warn, "error fetching deps: {~p, ~p}~n~p", [E, R, ST]),
            safe_get_install_deps(State, N - 1)
    end.

get_data(State) ->
    ProjectApps = rebar_state:project_apps(State),
    DepApps = rebar_state:all_deps(State),
    #{
        apps => [app_build_opts(App) || App <- ProjectApps],
        deps => [app_build_opts(App) || App <- DepApps],
        otp_lib_dir => list_to_binary(code:lib_dir()),
        source_root => list_to_binary(rebar_state:dir(State))
    }.

etf_build_info(State, RawOpts) ->
    Data = get_data(State),

    To = proplists:get_value(to, RawOpts),
    case To of
        undefined ->
            rebar_log:log(info, "Build info:~n", []),
            io:fwrite("~p.~n", [Data]);
        File ->
            ok = file:write_file(File, term_to_binary(Data)),
            rebar_log:log(info, "Build info written to: ~ts", [File])
    end.

write_output(Encoded, RawOpts, Name) ->
    To = proplists:get_value(to, RawOpts),
    case To of
        undefined ->
            rebar_log:log(info, "~s:", [Name]),
            io:fwrite("~s~n", [Encoded]);
        File ->
            ok = file:write_file(File, [Encoded, "\n"]),
            rebar_log:log(info, "~s written to: ~ts", [Name, File])
    end.

all_include_dirs(Apps, Deps) ->
    lists:concat(
        [
            [filename:join(Dir, SrcDir) || SrcDir <- SrcDirs] ++ IncludeDirs
         || #{include_dirs := IncludeDirs, src_dirs := SrcDirs, dir := Dir} <- Apps
        ] ++
            [
                IncludeDirs
             || #{include_dirs := IncludeDirs} <- Deps
            ]
    ).

els_build_info(State, RawOpts) ->
    #{deps := Deps, apps := Apps} = get_data(State),

    %% prepare data
    BaseDir = rebar_state:dir(State),
    IncludeDirs = [
        Dir
     || IncludeDir <- all_include_dirs(Apps, Deps),
        Dir <- [make_relative(BaseDir, IncludeDir), make_relative(BaseDir, enclosing_app_folder(IncludeDir))],
        filelib:is_dir(Dir)
    ],
    AppsDirs = [make_relative(BaseDir, AppDir) || #{dir := AppDir} <- Apps],

    %% extract feature config from exsiting config file
    Erlang_LS_Config = proplists:get_value(els_config, RawOpts),
    Config = maybe_get_existing_config(Erlang_LS_Config),

    %% prepare JSON term
    JSONData = [
        {<<"include_dirs">>, lists:usort(IncludeDirs)},
        {<<"apps_dirs">>, lists:usort(AppsDirs)}
        | Config
    ],
    Encoded = jsone:encode(JSONData, [{indent, 2}, native_forward_slash]),
    write_output(Encoded, RawOpts, <<"erlang_ls.config">>).

rl_build_info(State, RawOpts) ->
    Data = get_data(State),

    %% prepare data
    #{apps := Apps} = Data,
    BaseDir = rebar_state:dir(State),
    AppsDirs = [
        make_relative(BaseDir, AppDir)
     || #{
            dir := AppDir
        } <- Apps
    ],

    %% output
    Encoded = string:join(lists:map(fun binary_to_list/1, AppsDirs), "\n"),
    write_output(Encoded, RawOpts, <<"erlang_rl.config">>).

%% report a guess as to what files would be created by running `wiki_moduledocs extract'
%% in the DOCS file format used by fbsphinx.
%%
%% the result should be an over-estimate; not every module/app will necessarily be
%% successfully processed (and some will be generated/private); fbsphinx will leniently
%% ignore any missing files.  if TODO(T96305665) is ever implemented, this and the
%% associated linter should be removed.
%%
%% this is meant to be used with and support a single use case: updating the
%% `erl/docs/DOCS' file as part of an `arc lint'.  it assumes presence of specific
%% template vars in that file.  it assumes the presence of `erl/docs/DOCS.src' as a
%% template file.
wm_build_info(State, RawOpts) ->
    #{apps := Apps} = get_data(State),
    BaseDir = rebar_state:dir(State),

    %% find the source modules of each app:
    AppSources = wm_app_sources(BaseDir, Apps),

    %% convert each source path to its corresponding DOCS path, and check each appdir for
    %% presence of an existing readme to determine the final output readme name seen by
    %% fbsphinx:
    DocsPaths = wm_output_pathnames(AppSources),

    %% substitute the docs/readmes content for the relevant template variables:
    FinalContent = wm_populate_template(BaseDir, DocsPaths),

    write_output(FinalContent, RawOpts, <<"wiki_moduledocs DOCS">>).

%% given the build options for an app, return its name, its directory, and a list of its
%% source pathnames. each such source file has a corresponding entry in erl/docs/DOCS as
%% "erlang/AppName/modname.rst". each app has a README.md or README.rst in the same
%% location (if it had any modules).
-spec wm_app_sources(binary(), [app_build_opts()]) -> [{binary(), binary(), [binary()]}].
wm_app_sources(BaseDir, Apps) ->
    lists:foldl(
        fun(#{name := AppName, dir := AppDir, src_dirs := SrcDirs}, Acc) ->
            RelativeAppDir = make_relative(BaseDir, AppDir),
            %% currently, wiki_moduledocs ignores most/all non-toplevel apps; this ignores
            %% app paths containing more than a single "/":
            ShouldIgnore =
                case filename:split(RelativeAppDir) of
                    [<<".">>, _] ->
                        false;
                    [_] ->
                        false;
                    _ ->
                        rebar_log:log(debug, "ignoring appdir ~s", [RelativeAppDir]),
                        true
                end,
            case ShouldIgnore of
                true ->
                    Acc;
                false ->
                    [
                        {AppName, RelativeAppDir,
                            lists:append([
                                lists_to_binaries(
                                    filelib:wildcard(
                                        unicode:characters_to_list(
                                            filename:join([RelativeAppDir, SrcDir, "*.erl"])
                                        )
                                    )
                                )
                             || SrcDir <- SrcDirs
                            ])}
                        | Acc
                    ]
            end
        end,
        [],
        Apps
    ).

%% given the output of `wm_app_sources', produce a list of relative pathnames to be
%% handled by fbsphinx corresponding to module docs and app readmes.
-spec wm_output_pathnames([{AppName :: binary(), AppDir :: binary(), Sources :: [binary()]}]) ->
    {Docs :: [binary()], Readmes :: [binary()]}.
wm_output_pathnames(AppSources) ->
    {A, B} =
        lists:foldl(
            fun({AppName, AppDir, Sources}, {DocsAcc, ReadmesAcc}) ->
                DocsAcc1 = [
                    [
                        filename:join([
                            "erlang",
                            AppName,
                            [filename:basename(Source, ".erl"), ".rst"]
                        ])
                     || Source <- Sources
                    ]
                    | DocsAcc
                ],

                %% readmes are handled in their own section to work around an fbsphinx
                %% bug (parent paths must be created before same-named children):
                ReadmeExt =
                    case filelib:is_regular(filename:join(AppDir, "README.md")) of
                        true ->
                            ".md";
                        false ->
                            ".rst"
                    end,
                ReadmesAcc1 = [[filename:join(["erlang", AppName, "README"]), ReadmeExt] | ReadmesAcc],
                {DocsAcc1, ReadmesAcc1}
            end,
            {[], []},
            AppSources
        ),
    {lists:append(A), B}.

%% given the output of `wm_output_pathnames', produce the final corresponding DOCS content
%% by substituting into the static source template file.
-spec wm_populate_template(binary(), {[binary()], [binary()]}) -> binary().
wm_populate_template(BaseDir, {Docs, Readmes}) ->
    DocsSrc = filename:join([BaseDir, "docs", "DOCS.src"]),
    {ok, DocsSrcContent} = file:read_file(DocsSrc),

    %% items should be sorted, but readmes should occur before deeper children in order to
    %% work around an fbsphinx bug:
    Items = lists:sort(["erlang/README.rst" | Readmes]) ++ lists:sort(Docs),
    Srcs = ["[", lists:join(",\n          ", Items), "]"],

    %% substitute template variables for actual content. note: `re:split' with a fold is
    %% ~6x faster than using `re:replace'.
    Substitutions = #{<<"%%SRCS%%">> => Srcs},
    SplitPattern = ["(", lists:join("|", maps:keys(Substitutions)), ")"],
    SplitContent = re:split(DocsSrcContent, SplitPattern, [{return, binary}, unicode]),
    lists:foldl(
        fun(Part, Acc) ->
            case Substitutions of
                #{Part := Replacement} ->
                    [Acc, Replacement];
                _ ->
                    [Acc, Part]
            end
        end,
        [],
        SplitContent
    ).

make_relative(Base, Full) ->
    case string:split(Full, Base) of
        [Prefix, Relative] ->
            case string:is_empty(Prefix) of
                true ->
                    unicode:characters_to_binary([".", Relative]);
                _ ->
                    make_relative_error(Full, Base)
            end;
        _ ->
            make_relative_error(Full, Base)
    end.

enclosing_app_folder(Dir) ->
    filename:dirname(filename:dirname(Dir)).

make_relative_error(Full, Base) ->
    rebar_log:log(error, "the path ~s is not an extension of the base path ~s", [Full, Base]),
    throw(rebar_abort).

maybe_get_existing_config(undefined) ->
    [];
maybe_get_existing_config(File) ->
    case file:read_file(File) of
        {ok, Json} ->
            try
                Data = jsone:decode(Json),
                maps:to_list(Data)
            catch
                E:R ->
                    rebar_log:log(error, "error parsing erlang_ls config at ~s (~p)", [File, {E, R}]),
                    throw(rebar_abort)
            end;
        {error, _} = Error ->
            rebar_log:log(error, "could not open erlang_ls config at ~s (~p)", [File, Error]),
            throw(rebar_abort)
    end.

%% From rebar_compiler:context/1

-spec app_build_opts(rebar_app_info:t()) -> app_build_opts().
app_build_opts(AppInfo) ->
    Name = rebar_app_info:name(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    RebarOpts = rebar_app_info:opts(AppInfo),
    SrcDirs = rebar_dir:src_dirs(RebarOpts, ["src"]),
    ExistingSrcDirs = [Dir || Dir <- SrcDirs, ec_file:is_dir(filename:join(AppDir, Dir))],
    ExtraSrcDirs = rebar_dir:extra_src_dirs(RebarOpts),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    ErlOptIncludes = proplists:get_all_values(i, ErlOpts),
    InclDirs =
        [filename:join(AppDir, "include")] ++ [filename:absname(Dir) || Dir <- ErlOptIncludes],
    PTrans = proplists:get_all_values(parse_transform, ErlOpts),
    Macros = macros(ErlOpts),

    #{
        name => Name,
        dir => list_to_binary(AppDir),
        ebin => list_to_binary(EbinDir),
        src_dirs => lists_to_binaries(ExistingSrcDirs),
        extra_src_dirs => lists_to_binaries(ExtraSrcDirs),
        include_dirs => lists_to_binaries(InclDirs),
        macros => Macros,
        parse_transforms => PTrans
    }.

macros([{d, Name} | Rest]) -> [Name | macros(Rest)];
macros([{d, Name, Value} | Rest]) -> [{Name, Value} | macros(Rest)];
macros([_ | Rest]) -> macros(Rest);
macros([]) -> [].

lists_to_binaries(Strings) ->
    [list_to_binary(String) || String <- Strings].
