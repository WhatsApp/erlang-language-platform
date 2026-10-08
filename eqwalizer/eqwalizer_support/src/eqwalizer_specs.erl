%% % @format
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is dual-licensed under either the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree or the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree. You may select, at your option, one of the
%%% above-listed licenses.

-module(eqwalizer_specs).
-compile(warn_missing_spec).
% elp:ignore W0054 (no_nowarn_suppressions)
-compile([export_all, nowarn_export_all]).

-moduledoc """
This module provides a means to override specs from standard OTP libraries for
better type-checking with eqWAlizer.
""".

%% -------- application --------

-spec 'application:get_all_env'(App :: atom()) -> [{atom(), dynamic()}].
'application:get_all_env'(_) -> error(eqwalizer_specs).

-spec 'application:get_env'(Param :: atom()) -> undefined | {ok, dynamic()}.
'application:get_env'(_) -> error(eqwalizer_specs).

-spec 'application:get_env'(App :: atom(), Param :: atom()) ->
    undefined | {ok, dynamic()}.
'application:get_env'(_, _) -> error(eqwalizer_specs).

-spec 'application:get_env'(App :: atom(), Param :: atom(), Default) -> dynamic() | Default.
'application:get_env'(_, _, _) -> error(eqwalizer_specs).

-spec 'application:get_key'(Key :: atom()) -> undefined | {ok, dynamic()}.
'application:get_key'(_) -> error(eqwalizer_specs).

-spec 'application:get_key'(App :: atom(), Key :: atom()) -> undefined | {ok, dynamic()}.
'application:get_key'(_, _) -> error(eqwalizer_specs).

-spec 'application:info'() -> [{atom(), dynamic()}].
'application:info'() -> error(eqwalizer_specs).

%% -------- argparse --------

-type argparse_parser_options() :: #{
    prefixes => [char()],
    default => term(),
    progname => string() | atom(),
    command => [string()],
    columns => pos_integer()
}.

-spec 'argparse:run'(Args :: [string()], argparse:command(), argparse_parser_options()) -> dynamic().
'argparse:run'(_, _, _) -> error(eqwalizer_specs).

%% -------- array --------

-spec 'array:new'() -> array:array(none()).
'array:new'() -> error(eqwalizer_specs).

%% -------- code --------

% It's used in WASERVER with known application names.
% Having {'error', 'bad_name'} in result type is noisy.
-spec 'code:lib_dir'(atom()) -> file:filename().
'code:lib_dir'(_) -> error(eqwalizer_specs).

-spec 'code:priv_dir'(atom()) -> file:filename().
'code:priv_dir'(_) -> error(eqwalizer_specs).

%% -------- compile --------

-spec 'compile:forms'(compile:forms()) ->
    {ok, module(), binary()}
    | {ok, module(), binary(), dynamic()}
    | error
    | {error, dynamic(), dynamic()}.
'compile:forms'(_) -> error(eqwalizer_specs).

-spec 'compile:forms'(compile:forms(), [compile:option()]) ->
    {ok, module(), binary()}
    | {ok, module(), binary(), dynamic()}
    | error
    | {error, dynamic(), dynamic()}.
'compile:forms'(_, _) -> error(eqwalizer_specs).

%% -------- crypto --------

-type crypto_cipher_aead() ::
    aes_128_ccm
    | aes_192_ccm
    | aes_256_ccm
    | aes_ccm
    | aes_128_gcm
    | aes_192_gcm
    | aes_256_gcm
    | aes_gcm
    | chacha20_poly1305.

-spec 'crypto:crypto_one_time_aead'
    (Cipher, Key, IV, InText, AAD, EncryptTagLength, true) -> EncryptResult when
        Cipher :: crypto_cipher_aead(),
        Key :: iodata(),
        IV :: iodata(),
        InText :: iodata(),
        AAD :: iodata(),
        EncryptTagLength :: non_neg_integer(),
        EncryptResult :: {OutCryptoText, OutTag},
        OutCryptoText :: binary(),
        OutTag :: binary();
    (Cipher, Key, IV, InText, AAD, DecryptTag, false) -> DecryptResult when
        Cipher :: crypto_cipher_aead(),
        Key :: iodata(),
        IV :: iodata(),
        InText :: iodata(),
        AAD :: iodata(),
        DecryptTag :: iodata(),
        DecryptResult :: OutPlainText | error,
        OutPlainText :: binary().
'crypto:crypto_one_time_aead'(_, _, _, _, _, _, _) -> error(eqwalizer_specs).

%% -------- dets --------

-spec 'dets:lookup'(dets:tab_name(), term()) -> [Tuple] | {'error', term()} when Tuple :: dynamic().
'dets:lookup'(_, _) -> error(eqwalizer_specs).

%% -------- digraph --------

-spec 'digraph:add_vertex'(G :: digraph:graph(), V :: digraph:vertex(), Label :: digraph:label()) ->
    V :: dynamic().
'digraph:add_vertex'(_, _, _) -> error(eqwalizer_specs).

-spec 'digraph:edge'(G :: digraph:graph(), E :: digraph:edge()) ->
    {E :: dynamic(), V1 :: dynamic(), V2 :: dynamic(), Label :: dynamic()}
    | 'false'.
'digraph:edge'(_, _) -> error(eqwalizer_specs).

-spec 'digraph:vertex'(G :: digraph:graph(), V :: digraph:vertex()) ->
    {V :: dynamic(), Label :: dynamic()} | 'false'.
'digraph:vertex'(_, _) -> error(eqwalizer_specs).

%% -------- digraph_utils --------

-spec 'digraph_utils:cyclic_strong_components'(digraph:graph()) -> [[dynamic()]].
'digraph_utils:cyclic_strong_components'(_) -> error(eqwalizer_specs).

%% -------- epp_dodger --------

-spec 'epp_dodger:parse_file'(file:filename_all()) ->
    {ok, [erl_syntax:syntaxTree()]} | {error, erl_scan:error_info()}.
'epp_dodger:parse_file'(_) -> error(eqwalizer_specs).

%% -------- erl_parse --------

-spec 'erl_parse:map_anno'(fun((dynamic()) -> dynamic()), AST) -> AST.
'erl_parse:map_anno'(_, _) -> error(eqwalizer_specs).

-spec 'erl_parse:parse_term'([erl_scan:token()]) ->
    {ok, dynamic()} | {error, erl_parse:error_info()}.
'erl_parse:parse_term'(_) -> error(eqwalizer_specs).

%% -------- erl_syntax --------

-spec 'erl_syntax:concrete'(erl_syntax:syntaxTree()) -> dynamic().
'erl_syntax:concrete'(_) -> error(eqwalizer_specs).

-spec 'erl_syntax:revert'(erl_syntax:syntaxTree()) -> dynamic().
'erl_syntax:revert'(_) -> error(eqwalizer_specs).

%% -------- erl_syntax_lib --------

-spec 'erl_syntax_lib:fold'(
    fun((erl_syntax:syntaxTree(), Acc) -> Acc), Acc, erl_syntax:syntaxTree()
) -> Acc.
'erl_syntax_lib:fold'(_, _, _) -> error(eqwalizer_specs).

-spec 'erl_syntax_lib:fold_subtrees'(fun((erl_syntax:syntaxTree(), Acc) -> Acc), Acc, erl_syntax:syntaxTree()) -> Acc.
'erl_syntax_lib:fold_subtrees'(_, _, _) -> error(eqwalizer_specs).

%% -------- erlang --------

-spec 'erlang:abs'(number()) -> number().
'erlang:abs'(_) -> error(eqwalizer_specs).

-spec 'erlang:apply'(Fun, Args) -> Ret when
    Fun :: fun((...) -> Ret),
    Args :: [term()].
'erlang:apply'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:apply'(Module, Function, Args) -> dynamic() when
    Module :: module(),
    Function :: atom(),
    Args :: [term()].
'erlang:apply'(_, _, _) -> error(eqwalizer_specs).

-spec 'erlang:binary_to_term'(binary()) -> dynamic().
'erlang:binary_to_term'(_) -> error(eqwalizer_specs).

-spec 'erlang:element'(pos_integer(), tuple()) -> dynamic().
'erlang:element'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:fun_info'
    (function(), arity) -> {arity, non_neg_integer()};
    (function(), module) -> {module, module()};
    (function(), name) -> {name, atom()};
    (function(), type) -> {type, local | external};
    (function(), env) -> {env, [dynamic()]};
    (function(), index) -> {index, non_neg_integer() | undefined};
    (function(), new_index) -> {new_index, non_neg_integer() | undefined};
    (function(), new_uniq) -> {new_uniq, binary() | undefined};
    (function(), uniq) -> {uniq, integer() | undefined};
    (function(), pid) -> {pid, pid() | undefined}.
'erlang:fun_info'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:hd'([A, ...]) -> A.
'erlang:hd'(_) -> error(eqwalizer_specs).

-spec 'erlang:max'(A, B) -> A | B.
'erlang:max'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:min'(A, B) -> A | B.
'erlang:min'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:system_time'() -> pos_integer().
'erlang:system_time'() -> error(eqwalizer_specs).

-spec 'erlang:system_time'(erlang:time_unit()) -> pos_integer().
'erlang:system_time'(_) -> error(eqwalizer_specs).

-spec 'erlang:tuple_to_list'(tuple()) -> [dynamic()].
'erlang:tuple_to_list'(_) -> error(eqwalizer_specs).

-spec 'erlang:get'() -> [{dynamic(), dynamic()}].
'erlang:get'() -> error(eqwalizer_specs).

-spec 'erlang:get'(term()) -> dynamic().
'erlang:get'(_) -> error(eqwalizer_specs).

-spec 'erlang:put'(term(), term()) -> dynamic().
'erlang:put'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:erase'() -> [{dynamic(), dynamic()}].
'erlang:erase'() -> error(eqwalizer_specs).

-spec 'erlang:erase'(term()) -> dynamic().
'erlang:erase'(_) -> error(eqwalizer_specs).

-spec 'erlang:raise'(Class, Reason, Stacktrace) -> none() when
    Class :: 'error' | 'exit' | 'throw',
    Reason :: term(),
    Stacktrace :: [term()].
'erlang:raise'(_, _, _) -> error(eqwalizer_specs).

-spec 'erlang:send'(erlang:send_destination(), Msg) -> Msg.
'erlang:send'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:tl'([A]) -> [A].
'erlang:tl'(_) -> error(eqwalizer_specs).

-spec 'erlang:binary_to_term'(binary(), [safe | used]) -> dynamic().
'erlang:binary_to_term'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:list_to_existing_atom'(string()) -> eqwalizer:dynamic(atom()).
'erlang:list_to_existing_atom'(_) -> error(eqwalizer_specs).

-spec 'erlang:binary_to_existing_atom'(binary()) -> eqwalizer:dynamic(atom()).
'erlang:binary_to_existing_atom'(_) -> error(eqwalizer_specs).

-spec 'erlang:binary_to_existing_atom'(binary(), latin1 | unicode | utf8) -> eqwalizer:dynamic(atom()).
'erlang:binary_to_existing_atom'(_, _) -> error(eqwalizer_specs).

%% -------- erpc --------

-spec 'erpc:call'(Node, Fun) -> Result when
    Node :: node(),
    Fun :: fun(() -> Result).
'erpc:call'(_, _) -> error(eqwalizer_specs).

-spec 'erpc:call'(Node, Fun, TimeoutOrOptions) -> Result when
    Node :: node(),
    Fun :: fun(() -> Result),
    TimeoutOrOptions :: erpc:timeout_time() | #{timeout => erpc:timeout_time(), always_spawn => boolean()}.
'erpc:call'(_, _, _) -> error(eqwalizer_specs).

-spec 'erpc:call'(Node, Module, Function, Args) -> Result when
    Node :: node(),
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Result :: dynamic().
'erpc:call'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'erpc:call'(Node, Module, Function, Args, TimeoutOrOptions) -> Result when
    Node :: node(),
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    TimeoutOrOptions :: erpc:timeout_time() | #{timeout => erpc:timeout_time(), always_spawn => boolean()},
    Result :: dynamic().
'erpc:call'(_, _, _, _, _) -> error(eqwalizer_specs).

-spec 'erpc:multicall'(Nodes, Fun) -> Result when
    Nodes :: [atom()],
    Fun :: function(),
    Result :: dynamic().
'erpc:multicall'(_, _) -> error(eqwalizer_specs).

-spec 'erpc:multicall'(Nodes, Module, Function, Args) -> [{ok, Res} | Error] when
    Nodes :: [node()],
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Res :: dynamic(),
    Error ::
        {throw, Throw :: term()}
        | {exit, {exception, Reason :: term()}}
        | {error, {exception, Reason :: term(), StackTrace :: [Stack]}}
        | {exit, {signal, Reason :: term()}}
        | {error, {erpc, Reason :: term()}},
    Stack ::
        {
            Module :: atom(),
            Function :: atom(),
            Arity :: arity() | (Args :: [term()]),
            Location :: [
                {file, Filename :: string()}
                | {line, Line :: pos_integer()}
            ]
        }.
'erpc:multicall'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'erpc:multicall'(Nodes, Module, Function, Args, Timeout) -> [{ok, Res} | Error] when
    Nodes :: [node()],
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Res :: dynamic(),
    Timeout :: erpc:timeout_time(),
    Error ::
        {throw, Throw :: term()}
        | {exit, {exception, Reason :: term()}}
        | {error, {exception, Reason :: term(), StackTrace :: [Stack]}}
        | {exit, {signal, Reason :: term()}}
        | {error, {erpc, Reason :: term()}},
    Stack ::
        {
            Module :: atom(),
            Function :: atom(),
            Arity :: arity() | (Args :: [term()]),
            Location :: [
                {file, Filename :: string()}
                | {line, Line :: pos_integer()}
            ]
        }.
'erpc:multicall'(_, _, _, _, _) -> error(eqwalizer_specs).

-spec 'erpc:receive_response'(RequestId, Timeout) -> Result when
    RequestId :: erpc:request_id(),
    Timeout :: erpc:timeout_time(),
    Result :: dynamic().
'erpc:receive_response'(_, _) -> error(eqwalizer_specs).

%% -------- ets --------

-spec 'ets:first'(ets:table()) -> dynamic().
'ets:first'(_) -> error(eqwalizer_specs).

-spec 'ets:first_lookup'(Table) -> {Key, [Object]} | '$end_of_table' when
    Table :: ets:table(),
    Key :: dynamic(),
    Object :: tuple().
'ets:first_lookup'(_) -> error(eqwalizer_specs).

-spec 'ets:foldl'(Function, Acc, Table) -> Acc when
    Function :: fun((Element :: dynamic(), Acc) -> Acc),
    Table :: ets:table().
'ets:foldl'(_, _, _) -> error(eqwalizer_specs).

-spec 'ets:foldr'(Function, Acc, Table) -> Acc when
    Function :: fun((Element :: dynamic(), Acc) -> Acc),
    Table :: ets:table().
'ets:foldr'(_, _, _) -> error(eqwalizer_specs).

-spec 'ets:info'
    (ets:table(), compressed | decentralized_counters | fixed | named_table | read_concurrency | write_concurrency) ->
        boolean();
    (ets:table(), binary) -> list();
    (ets:table(), heir) -> pid() | none;
    (ets:table(), id) -> ets:tid();
    (ets:table(), keypos | memory | size) -> non_neg_integer();
    (ets:table(), name) -> atom();
    (ets:table(), node) -> node();
    (ets:table(), owner) -> pid();
    (ets:table(), safe_fixed | safe_fixed_monotonic_time) -> tuple() | false;
    (ets:table(), stats) -> tuple();
    (ets:table(), protection) -> ets:table_access();
    (ets:table(), type) -> ets:table_type().
'ets:info'(_, _) -> error(eqwalizer_specs).

-spec 'ets:next'(ets:table(), term()) -> dynamic().
'ets:next'(_, _) -> error(eqwalizer_specs).

-spec 'ets:next_lookup'(ets:table(), term()) -> {dynamic(), list()} | '$end_of_table'.
'ets:next_lookup'(_, _) -> error(eqwalizer_specs).

-spec 'ets:prev'(ets:table(), term()) -> dynamic().
'ets:prev'(_, _) -> error(eqwalizer_specs).

-spec 'ets:prev_lookup'(ets:table(), term()) -> {dynamic(), list()} | '$end_of_table'.
'ets:prev_lookup'(_, _) -> error(eqwalizer_specs).

-spec 'ets:last'(ets:table()) -> dynamic() | '$end_of_table'.
'ets:last'(_) -> error(eqwalizer_specs).

-spec 'ets:lookup'(ets:table(), term()) -> [dynamic()].
'ets:lookup'(_, _) -> error(eqwalizer_specs).

-spec 'ets:lookup_element'(ets:table(), term(), pos_integer()) -> dynamic().
'ets:lookup_element'(_, _, _) -> error(eqwalizer_specs).

-spec 'ets:lookup_element'(ets:table(), term(), pos_integer(), Default) -> dynamic() | Default.
'ets:lookup_element'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'ets:match'(ets:table(), ets:match_pattern()) -> [[dynamic()]].
'ets:match'(_, _) -> error(eqwalizer_specs).

-spec 'ets:select'(EtsContinuation) ->
    {[dynamic()], EtsContinuation} | '$end_of_table'
when
    EtsContinuation :: dynamic().
'ets:select'(_) -> error(eqwalizer_specs).

-spec 'ets:select'(ets:table(), ets:match_spec()) -> [dynamic()].
'ets:select'(_, _) -> error(eqwalizer_specs).

-spec 'ets:select'(ets:table(), ets:match_spec(), pos_integer()) ->
    {[dynamic()], EtsContinuation} | '$end_of_table'
when
    EtsContinuation :: dynamic().
'ets:select'(_, _, _) -> error(eqwalizer_specs).

-spec 'ets:select_reverse'(ets:table(), ets:match_spec()) -> [dynamic()].
'ets:select_reverse'(_, _) -> error(eqwalizer_specs).

-spec 'ets:select_reverse'(ets:table(), ets:match_spec(), pos_integer()) ->
    {[dynamic()], EtsContinuation} | '$end_of_table'
when
    EtsContinuation :: dynamic().
'ets:select_reverse'(_, _, _) -> error(eqwalizer_specs).

-spec 'ets:tab2list'(ets:table()) -> [dynamic()].
'ets:tab2list'(_) -> error(eqwalizer_specs).

-spec 'ets:take'(ets:table(), term()) -> [dynamic()].
'ets:take'(_, _) -> error(eqwalizer_specs).

-spec 'ets:test_ms'(tuple(), ets:match_spec()) ->
    {ok, dynamic()} | {error, [{warning | error, string()}]}.
'ets:test_ms'(_, _) -> error(eqwalizer_specs).

-spec 'ets:update_counter'(Table, Key, UpdateOp | [UpdateOp] | Incr) -> eqwalizer:dynamic(Result | [Result]) when
    Table :: ets:table(),
    Key :: term(),
    UpdateOp :: {Pos, Incr} | {Pos, Incr, Threshold, SetValue},
    Pos :: integer(),
    Incr :: integer(),
    Threshold :: integer(),
    SetValue :: integer(),
    Result :: integer().
'ets:update_counter'(_, _, _) -> error(eqwalizer_specs).

-spec 'ets:update_counter'(Table, Key, UpdateOp | Incr | [UpdateOp], Default) ->
    eqwalizer:dynamic(Result | [Result])
when
    Table :: ets:table(),
    Key :: term(),
    UpdateOp :: {Pos, Incr} | {Pos, Incr, Threshold, SetValue},
    Pos :: integer(),
    Incr :: integer(),
    Threshold :: integer(),
    SetValue :: integer(),
    Result :: integer(),
    Default :: tuple().
'ets:update_counter'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'ets:match_object'(ets:table(), ets:match_pattern()) -> [dynamic()].
'ets:match_object'(_, _) -> error(eqwalizer_specs).

%% -------- file --------

-spec 'file:consult'(Filename) -> {ok, Terms} | {error, Reason} when
    Filename :: file:name_all(),
    Terms :: [dynamic()],
    Reason ::
        file:posix()
        | badarg
        | terminated
        | system_limit
        | {Line :: integer(), Mod :: module(), Term :: term()}.
'file:consult'(_) -> error(eqwalizer_specs).

-spec 'file:list_dir'(Dir) -> {ok, Filenames} | {error, Reason} when
    Dir :: file:name_all(),
    Filenames :: [string()],
    Reason :: file:posix() | badarg.
'file:list_dir'(_) -> error(eqwalizer_specs).

-spec 'file:list_dir_all'(Dir) -> {ok, Filenames} | {error, Reason} when
    Dir :: file:name_all(),
    Filenames :: [string()],
    Reason :: file:posix() | badarg.
'file:list_dir_all'(_) -> error(eqwalizer_specs).

-spec 'file:pread'(IoDevice, LocNums) -> {ok, DataL} | eof | {error, Reason} when
    IoDevice :: file:io_device(),
    LocNums :: [{Location :: file:location(), Number :: non_neg_integer()}],
    DataL :: [Data],
    Data :: eqwalizer:dynamic(string() | binary()) | eof,
    Reason :: file:posix() | badarg | terminated.
'file:pread'(_, _) -> error(eqwalizer_specs).

-spec 'file:read_line'(IoDevice) -> {ok, Data} | eof | {error, Reason} when
    IoDevice :: file:io_device() | io:device(),
    Data :: eqwalizer:dynamic(string() | binary()),
    Reason ::
        file:posix()
        | badarg
        | terminated
        | {no_translation, unicode, latin1}.
'file:read_line'(_) -> error(eqwalizer_specs).

%% -------- filelib --------

-spec 'filelib:fold_files'(Dir, RegExp, Recursive, Fun, Acc) -> Acc when
    Dir :: file:name_all(),
    RegExp :: string(),
    Recursive :: boolean(),
    Fun :: fun((F :: file:filename(), Acc) -> Acc).
'filelib:fold_files'(_, _, _, _, _) -> error(eqwalizer_specs).

-spec 'filelib:safe_relative_path'(file:name_all(), file:name_all()) -> dynamic().
'filelib:safe_relative_path'(_, _) -> error(eqwalizer_specs).

%% -------- filename --------

-spec 'filename:basename'
    (string()) -> string();
    (binary()) -> binary().
'filename:basename'(_) -> error(eqwalizer_specs).

-spec 'filename:dirname'
    (string()) -> string();
    (binary()) -> binary().
'filename:dirname'(_) -> error(eqwalizer_specs).

-spec 'filename:extension'
    (string()) -> string();
    (binary()) -> binary().
'filename:extension'(_) -> error(eqwalizer_specs).

-spec 'filename:rootname'
    (string()) -> string();
    (binary()) -> binary().
'filename:rootname'(_) -> error(eqwalizer_specs).

-spec 'filename:rootname'
    (string(), string()) -> string();
    (binary(), binary()) -> binary().
'filename:rootname'(_, _) -> error(eqwalizer_specs).

-spec 'filename:basename'
    (string(), string()) -> string();
    (binary(), binary()) -> binary();
    (binary(), string()) -> binary();
    (string(), binary()) -> binary().
'filename:basename'(_, _) -> error(eqwalizer_specs).

-spec 'filename:split'
    (io_lib:chars() | atom()) -> [string()];
    (binary()) -> [binary()].
'filename:split'(_) -> error(eqwalizer_specs).

-spec 'filename:absname_join'(file:name_all(), file:name_all()) -> dynamic().
'filename:absname_join'(_, _) -> error(eqwalizer_specs).

%% -------- gen_event --------

-type gen_event_emgr_ref() ::
    atom() | {atom(), atom()} | {'global', term()} | {'via', atom(), term()} | pid().

-spec 'gen_event:call'(gen_event_emgr_ref(), gen_event:handler(), term()) -> dynamic().
'gen_event:call'(_, _, _) -> error(eqwalizer_specs).

-spec 'gen_event:call'(gen_event_emgr_ref(), gen_event:handler(), term(), timeout()) -> dynamic().
'gen_event:call'(_, _, _, _) -> error(eqwalizer_specs).

%% -------- gen_server --------

-spec 'gen_server:call'(gen_server:server_ref(), term()) -> dynamic().
'gen_server:call'(_, _) -> error(eqwalizer_specs).

-spec 'gen_server:call'(gen_server:server_ref(), term(), timeout()) -> dynamic().
'gen_server:call'(_, _, _) -> error(eqwalizer_specs).

-spec 'gen_server:multi_call'(Name :: atom(), Request :: term()) ->
    {[{node(), dynamic()}], [node()]}.
'gen_server:multi_call'(_, _) -> error(eqwalizer_specs).

-spec 'gen_server:multi_call'(Nodes :: [node()], Name :: atom(), Request :: term()) ->
    {[{node(), dynamic()}], [node()]}.
'gen_server:multi_call'(_, _, _) -> error(eqwalizer_specs).

-spec 'gen_server:multi_call'(Nodes :: [node()], Name :: atom(), Request :: term(), Timeout :: timeout()) ->
    {[{node(), dynamic()}], [node()]}.
'gen_server:multi_call'(_, _, _, _) -> error(eqwalizer_specs).

%% -------- gen_statem --------

-spec 'gen_statem:call'(gen_statem:server_ref(), term()) -> dynamic().
'gen_statem:call'(_, _) -> error(eqwalizer_specs).

-spec 'gen_statem:call'(gen_statem:server_ref(), term(), Timeout) -> dynamic() when
    Timeout :: timeout() | {clean_timeout, timeout()} | {dirty_timeout, timeout()}.
'gen_statem:call'(_, _, _) -> error(eqwalizer_specs).

-spec 'gen_statem:check_response'(Msg, ReqIdCollection, Delete) -> Result when
    Msg :: term(),
    ReqIdCollection :: gen_statem:request_id_collection(),
    Delete :: boolean(),
    Response ::
        {reply, Reply :: dynamic()}
        | {error, {Reason :: dynamic(), gen_statem:server_ref()}},
    Result ::
        {Response, Label :: dynamic(), NewReqIdCollection :: gen_statem:request_id_collection()}
        | 'no_request'
        | 'no_reply'.
'gen_statem:check_response'(_, _, _) -> error(eqwalizer_specs).

%% -------- gb_sets --------

-spec 'gb_sets:empty'() -> gb_sets:set(none()).
'gb_sets:empty'() -> error(eqwalizer_specs).

-spec 'gb_sets:new'() -> gb_sets:set(none()).
'gb_sets:new'() -> error(eqwalizer_specs).

%% -------- gb_trees --------

-spec 'gb_trees:empty'() -> gb_trees:tree(none(), none()).
'gb_trees:empty'() -> error(eqwalizer_specs).

-spec 'gb_trees:take'(Key, Tree) -> {Value, Tree} when Tree :: gb_trees:tree(Key, Value).
'gb_trees:take'(_, _) -> error(eqwalizer_specs).

%% -------- httpc --------

-type httpc_method() :: head | get | put | patch | post | trace | options | delete.

-type httpc_header() :: {Field :: [byte()], Value :: binary() | iolist()}.

-type httpc_request_body() ::
    iolist()
    | binary()
    | {fun((Acc :: term()) -> eof | {ok, iolist(), Acc :: term()}), Acc :: term()}
    | {chunkify, fun((Acc :: term()) -> eof | {ok, iolist(), Acc :: term()}), Acc :: term()}.

-type httpc_request() ::
    {uri_string:uri_string(), [httpc_header()]}
    | {uri_string:uri_string(), [httpc_header()], ContentType :: string(), httpc_request_body()}.

-type httpc_result() ::
    {
        StatusLine :: {HttpVersion :: string(), StatusCode :: non_neg_integer(), string()},
        [httpc_header()],
        Body :: string() | binary()
    }
    | {StatusCode :: non_neg_integer(), Body :: string() | binary()}
    | saved_to_file
    | RequestId :: dynamic().

-spec 'httpc:request'(uri_string:uri_string()) -> {ok, httpc_result()} | {error, term()}.
'httpc:request'(_) -> error(eqwalizer_specs).

-spec 'httpc:request'(uri_string:uri_string(), Profile :: atom() | pid()) ->
    {ok, httpc_result()} | {error, term()}.
'httpc:request'(_, _) -> error(eqwalizer_specs).

-spec 'httpc:request'(Method, Request, HttpOptions, Options) -> {ok, httpc_result()} | {error, term()} when
    Method :: httpc_method(),
    Request :: httpc_request(),
    HttpOptions :: [{atom(), term()}],
    Options :: [{atom(), term()}].
'httpc:request'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'httpc:request'(Method, Request, HttpOptions, Options, Profile) -> {ok, httpc_result()} | {error, term()} when
    Method :: httpc_method(),
    Request :: httpc_request(),
    HttpOptions :: [{atom(), term()}],
    Options :: [{atom(), term()}],
    Profile :: atom() | pid().
'httpc:request'(_, _, _, _, _) -> error(eqwalizer_specs).

%% -------- inets --------

-spec 'inets:start'(httpc | httpd, dynamic()) -> {ok, pid()} | {error, dynamic()}.
'inets:start'(_, _) -> error(eqwalizer_specs).

%% -------- io --------

-spec 'io:fread'(Prompt, Format) -> Result when
    Prompt :: unicode:chardata(),
    Format :: io:format(),
    Result :: {'ok', Terms :: [dynamic()]} | 'eof' | {'error', What :: dynamic()}.
'io:fread'(_, _) -> error(eqwalizer_specs).

%% -------- json --------

-spec 'json:decode'(binary()) -> dynamic().
'json:decode'(_) -> error(eqwalizer_specs).

%% -------- jsone --------

-spec 'jsone:decode'(binary()) -> dynamic().
'jsone:decode'(_) -> error(eqwalizer_specs).

-spec 'jsone:decode'(binary(), list()) -> dynamic().
'jsone:decode'(_, _) -> error(eqwalizer_specs).

%% -------- lists --------

-spec 'lists:all'(fun((T) -> boolean()), [T]) -> boolean().
'lists:all'(_, _) -> error(eqwalizer_specs).

-spec 'lists:any'(fun((T) -> boolean()), [T]) -> boolean().
'lists:any'(_, _) -> error(eqwalizer_specs).

-spec 'lists:append'([[T]]) -> [T].
'lists:append'(_) -> error(eqwalizer_specs).

-spec 'lists:append'([T], [T]) -> [T].
'lists:append'(_, _) -> error(eqwalizer_specs).

-spec 'lists:delete'(term(), [T]) -> [T].
'lists:delete'(_, _) -> error(eqwalizer_specs).

-spec 'lists:droplast'([T]) -> [T].
'lists:droplast'(_) -> error(eqwalizer_specs).

-spec 'lists:dropwhile'(fun((T) -> boolean()), [T]) -> [T].
'lists:dropwhile'(_, _) -> error(eqwalizer_specs).

-spec 'lists:duplicate'(non_neg_integer(), T) -> [T].
'lists:duplicate'(_, _) -> error(eqwalizer_specs).

-spec 'lists:enumerate'([A]) -> [{integer(), A}].
'lists:enumerate'(_) -> error(eqwalizer_specs).

-spec 'lists:filter'(fun((T) -> boolean()), [T]) -> [T].
'lists:filter'(_, _) -> error(eqwalizer_specs).

-spec 'lists:filtermap'(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].
'lists:filtermap'(_, _) -> error(eqwalizer_specs).

-spec 'lists:flatmap'(fun((A) -> [B]), [A]) -> [B].
'lists:flatmap'(_, _) -> error(eqwalizer_specs).

-spec 'lists:flatlength'([term()]) -> non_neg_integer().
'lists:flatlength'(_) -> error(eqwalizer_specs).

-spec 'lists:foldl'(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
'lists:foldl'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:foldr'(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
'lists:foldr'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:foreach'(fun((T) -> term()), [T]) -> ok.
'lists:foreach'(_, _) -> error(eqwalizer_specs).

-spec 'lists:join'(T, [T]) -> [T].
'lists:join'(_, _) -> error(eqwalizer_specs).

-spec 'lists:keydelete'(Key :: term(), N :: pos_integer(), [Tuple]) -> [Tuple].
'lists:keydelete'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keyfind'(Key :: term(), N :: pos_integer(), [Tuple]) -> Tuple | false.
'lists:keyfind'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keyreplace'(Key :: term(), N :: pos_integer(), [Tuple], Tuple) -> [Tuple].
'lists:keyreplace'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'lists:keysearch'(Key :: term(), N :: pos_integer(), [Tuple]) -> {value, Tuple} | false.
'lists:keysearch'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keytake'(Key :: term(), N :: pos_integer(), [Tuple]) ->
    {value, Tuple, [Tuple]} | false.
'lists:keytake'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:last'([T]) -> T.
'lists:last'(_) -> error(eqwalizer_specs).

-spec 'lists:map'(fun((A) -> B), [A]) -> [B].
'lists:map'(_, _) -> error(eqwalizer_specs).

-spec 'lists:mapfoldl'(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
'lists:mapfoldl'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:mapfoldr'(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
'lists:mapfoldr'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:max'([T]) -> T.
'lists:max'(_) -> error(eqwalizer_specs).

-spec 'lists:member'(T, [T]) -> boolean().
'lists:member'(_, _) -> error(eqwalizer_specs).

-spec 'lists:merge'([[T]]) -> [T].
'lists:merge'(_) -> error(eqwalizer_specs).

-spec 'lists:merge'([X], [Y]) -> [X | Y].
'lists:merge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:merge'(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
'lists:merge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:merge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:merge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:min'([T]) -> T.
'lists:min'(_) -> error(eqwalizer_specs).

-spec 'lists:nth'(pos_integer(), [T]) -> T.
'lists:nth'(_, _) -> error(eqwalizer_specs).

-spec 'lists:nthtail'(pos_integer(), [T]) -> [T].
'lists:nthtail'(_, _) -> error(eqwalizer_specs).

-spec 'lists:partition'(fun((T) -> boolean()), [T]) -> {[T], [T]}.
'lists:partition'(_, _) -> error(eqwalizer_specs).

-spec 'lists:prefix'([T], [T]) -> boolean().
'lists:prefix'(_, _) -> error(eqwalizer_specs).

-spec 'lists:reverse'([T]) -> [T].
'lists:reverse'(_) -> error(eqwalizer_specs).

-spec 'lists:reverse'([T], [T]) -> [T].
'lists:reverse'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rmerge'([X], [Y]) -> [X | Y].
'lists:rmerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rmerge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:rmerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge'([X], [Y]) -> [X | Y].
'lists:rumerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge'(fun((X, Y) -> boolean()), [X], [Y]) -> [(X | Y)].
'lists:rumerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:rumerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:search'(fun((T) -> boolean()), [T]) -> {value, T} | false.
'lists:search'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sort'([T]) -> [T].
'lists:sort'(_) -> error(eqwalizer_specs).

-spec 'lists:sort'(fun((T, T) -> boolean()), [T]) -> [T].
'lists:sort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:split'(non_neg_integer(), [T]) -> {[T], [T]}.
'lists:split'(_, _) -> error(eqwalizer_specs).

-spec 'lists:splitwith'(fun((T) -> boolean()), [T]) -> {[T], [T]}.
'lists:splitwith'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sublist'([T], Len :: non_neg_integer()) -> [T].
'lists:sublist'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sublist'([T], Start :: pos_integer(), Len :: non_neg_integer()) -> [T].
'lists:sublist'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:subtract'([T], [T]) -> [T].
'lists:subtract'(_, _) -> error(eqwalizer_specs).

-spec 'lists:suffix'([T], [T]) -> boolean().
'lists:suffix'(_, _) -> error(eqwalizer_specs).

-spec 'lists:takewhile'(fun((T) -> boolean()), [T]) -> [T].
'lists:takewhile'(_, _) -> error(eqwalizer_specs).

-spec 'lists:umerge'([[T]]) -> [T].
'lists:umerge'(_) -> error(eqwalizer_specs).

-spec 'lists:umerge'([A], [B]) -> [A | B].
'lists:umerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:umerge'(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
'lists:umerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:umerge3'([A], [B], [C]) -> [A | B | C].
'lists:umerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:uniq'([A]) -> [A].
'lists:uniq'(_) -> error(eqwalizer_specs).

-spec 'lists:unzip'([{A, B}]) -> {[A], [B]}.
'lists:unzip'(_) -> error(eqwalizer_specs).

-spec 'lists:unzip3'([{A, B, C}]) -> {[A], [B], [C]}.
'lists:unzip3'(_) -> error(eqwalizer_specs).

-spec 'lists:usort'([T]) -> [T].
'lists:usort'(_) -> error(eqwalizer_specs).

-spec 'lists:usort'(fun((T, T) -> boolean()), [T]) -> [T].
'lists:usort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zf'(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].
'lists:zf'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zip'([A], [B]) -> [{A, B}].
'lists:zip'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zip3'([A], [B], [C]) -> [{A, B, C}].
'lists:zip3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith'(fun((X, Y) -> T), [X], [Y]) -> [T].
'lists:zipwith'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith3'(fun((X, Y, Z) -> T), [X], [Y], [Z]) -> [T].
'lists:zipwith3'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'lists:enumerate'(integer(), [A]) -> [{integer(), A}].
'lists:enumerate'(_, _) -> error(eqwalizer_specs).

-spec 'lists:enumerate'(integer(), integer(), [A]) -> [{integer(), A}].
'lists:enumerate'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keymember'(Key :: term(), N :: pos_integer(), [term()]) -> boolean().
'lists:keymember'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keymerge'(pos_integer(), [Tuple1], [Tuple2]) -> [Tuple1 | Tuple2].
'lists:keymerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keysort'(pos_integer(), [Tuple]) -> [Tuple].
'lists:keysort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:ukeymerge'(pos_integer(), [Tuple1], [Tuple2]) -> [Tuple1 | Tuple2].
'lists:ukeymerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:ukeysort'(pos_integer(), [Tuple]) -> [Tuple].
'lists:ukeysort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:uniq'(fun((A) -> term()), [A]) -> [A].
'lists:uniq'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zip'([A], [B], 'fail' | 'trim' | {'pad', {DefaultA, DefaultB}}) ->
    [{A | DefaultA, B | DefaultB}].
'lists:zip'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:zip3'([A], [B], [C], 'fail' | 'trim' | {'pad', {DefaultA, DefaultB, DefaultC}}) ->
    [{A | DefaultA, B | DefaultB, C | DefaultC}].
'lists:zip3'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith'(
    fun((X | DefaultX, Y | DefaultY) -> T),
    [X],
    [Y],
    'fail' | 'trim' | {'pad', {DefaultX, DefaultY}}
) -> [T].
'lists:zipwith'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith3'(
    fun((X | DefaultX, Y | DefaultY, Z | DefaultZ) -> T),
    [X],
    [Y],
    [Z],
    'fail' | 'trim' | {'pad', {DefaultX, DefaultY, DefaultZ}}
) -> [T].
'lists:zipwith3'(_, _, _, _, _) -> error(eqwalizer_specs).

%% -------- logger --------

-spec 'logger:log'
    (Level, StringOrReport, Metadata) -> ok when
        Level :: logger:level(),
        StringOrReport :: unicode:chardata() | logger:report(),
        Metadata :: logger:metadata();
    (Level, Format, Args) -> ok when
        Level :: logger:level(),
        Format :: io:format(),
        Args :: [term()];
    (Level, Fun, FunArgs) -> ok when
        Level :: logger:level(),
        Fun :: logger:msg_fun(),
        FunArgs :: term().
'logger:log'(_, _, _) -> error(eqwalizer_specs).

%% -------- logger_filters --------

-spec 'logger_filters:domain'(logger:log_event(), term()) -> logger:filter_return().
'logger_filters:domain'(_, _) -> error(eqwalizer_specs).

-spec 'logger_filters:progress'(logger:log_event(), term()) -> logger:filter_return().
'logger_filters:progress'(_, _) -> error(eqwalizer_specs).

%% -------- logger_formatter --------

-spec 'logger_formatter:check_config'(Config) -> ok | {error, term()} when
    Config :: map().
'logger_formatter:check_config'(_) -> error(eqwalizer_specs).

%% -------- maps --------

-spec 'maps:find'(Key, #{Key => Value}) -> {ok, Value} | error.
'maps:find'(_, _) -> error(eqwalizer_specs).

-spec 'maps:from_list'([{Key, Value}]) -> #{Key => Value}.
'maps:from_list'(_) -> error(eqwalizer_specs).

-spec 'maps:groups_from_list'(fun((T) -> Key), [T]) -> #{Key => [T]}.
'maps:groups_from_list'(_, _) -> error(eqwalizer_specs).

-spec 'maps:groups_from_list'(fun((T) -> Key), fun((T) -> Value), [T]) -> #{Key => [Value]}.
'maps:groups_from_list'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:merge'(map(), map()) -> map().
'maps:merge'(_, _) -> error(eqwalizer_specs).

-spec 'maps:put'(Key, Value, #{Key => Value}) -> #{Key => Value}.
'maps:put'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:remove'(term(), #{Key => Value}) -> #{Key => Value}.
'maps:remove'(_, _) -> error(eqwalizer_specs).

-spec 'maps:take'(Key :: term(), map()) -> {Value :: dynamic(), map()} | error.
'maps:take'(_, _) -> error(eqwalizer_specs).

-spec 'maps:update'(Key :: term(), Value :: term(), map()) -> map().
'maps:update'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:update_with'(Key :: term(), fun(), map()) -> map().
'maps:update_with'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:update_with'(Key :: term(), fun(), Init :: term(), map()) -> map().
'maps:update_with'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'maps:from_keys'([Key], Value) -> #{Key => Value}.
'maps:from_keys'(_, _) -> error(eqwalizer_specs).

%% -------- mod_esi --------

-spec 'mod_esi:deliver'(SessionID, Data) -> ok | {error, Reason} when
    SessionID :: term(),
    Data :: iodata(),
    Reason :: bad_sessionID.
'mod_esi:deliver'(_, _) -> error(eqwalizer_specs).

%% -------- orddict --------

-spec 'orddict:new'() -> orddict:orddict(none(), none()).
'orddict:new'() -> error(eqwalizer_specs).

-spec 'orddict:append'(Key, Value, Orddict1) -> Orddict2 when
    Orddict1 :: orddict:orddict(Key, [Value]),
    Orddict2 :: orddict:orddict(Key, [Value]).
'orddict:append'(_, _, _) -> error(eqwalizer_specs).

-spec 'orddict:take'(Key, Orddict) -> {Value, Orddict1} | error when
    Orddict :: orddict:orddict(Key, Value),
    Orddict1 :: orddict:orddict(Key, Value),
    Key :: term(),
    Value :: dynamic().
'orddict:take'(_, _) -> error(eqwalizer_specs).

%% -------- ordsets --------

-spec 'ordsets:subtract'(ordsets:ordset(T), ordsets:ordset(term())) -> ordsets:ordset(T).
'ordsets:subtract'(_, _) -> error(eqwalizer_specs).

-spec 'ordsets:fold'(fun((T, Acc) -> Acc), Acc, ordsets:ordset(T)) -> Acc.
'ordsets:fold'(_, _, _) -> error(eqwalizer_specs).

-spec 'ordsets:intersection'(ordsets:ordset(T), ordsets:ordset(T)) -> ordsets:ordset(T).
'ordsets:intersection'(_, _) -> error(eqwalizer_specs).

%% -------- persistent_term --------

-spec 'persistent_term:get'(term()) -> dynamic().
'persistent_term:get'(_) -> error(eqwalizer_specs).

-spec 'persistent_term:get'(term(), term()) -> dynamic().
'persistent_term:get'(_, _) -> error(eqwalizer_specs).

%% -------- proc_lib --------

-spec 'proc_lib:start_link'(Module, Function, Args) -> Ret when
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Ret :: dynamic().
'proc_lib:start_link'(_, _, _) -> error(eqwalizer_specs).

-spec 'proc_lib:get_label'(Pid) -> undefined | dynamic() when
    Pid :: pid().
'proc_lib:get_label'(_) -> error(eqwalizer_specs).

%% -------- proplists --------

-spec 'proplists:delete'(term(), [A]) -> [A].
'proplists:delete'(_, _) -> error(eqwalizer_specs).

-spec 'proplists:get_all_values'(term(), [term()]) -> [dynamic()].
'proplists:get_all_values'(_, _) -> error(eqwalizer_specs).

-spec 'proplists:get_keys'([term()]) -> [dynamic()].
'proplists:get_keys'(_) -> error(eqwalizer_specs).

-spec 'proplists:get_value'(term(), [term()]) -> dynamic().
'proplists:get_value'(_, _) -> error(eqwalizer_specs).

-spec 'proplists:get_value'(term(), [term()], term()) -> dynamic().
'proplists:get_value'(_, _, _) -> error(eqwalizer_specs).

-spec 'proplists:from_map'(#{K => V}) -> [{K, V}].
'proplists:from_map'(_) -> error(eqwalizer_specs).

%% -------- public_key --------

-spec 'public_key:der_decode'(public_key:asn1_type(), public_key:der_encoded()) ->
    dynamic().
'public_key:der_decode'(_, _) -> error(eqwalizer_specs).

-spec 'public_key:pem_entry_decode'(public_key:pem_entry()) -> dynamic().
'public_key:pem_entry_decode'(_) -> error(eqwalizer_specs).

%% -------- queue --------
-spec 'queue:new'() -> queue:queue(none()).
'queue:new'() -> error(eqwalizer_specs).

-spec 'queue:fold'(fun((Item, Acc) -> Acc), Acc, queue:queue(Item)) -> Acc.
'queue:fold'(_, _, _) -> error(eqwalizer_specs).

%% -------- peer --------

-spec 'peer:call'(
    Dest :: pid(),
    Module :: module(),
    Function :: atom(),
    Args :: [term()]
) -> Result :: dynamic().
'peer:call'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'peer:call'(
    Dest :: pid(),
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Timeout :: timeout()
) -> Result :: dynamic().
'peer:call'(_, _, _, _, _) -> error(eqwalizer_specs).

%% -------- re --------

-spec 're:run'(Subject, RE) ->
    {match, dynamic()} | match | nomatch | {error, dynamic()}
when
    Subject :: iodata() | unicode:charlist(),
    RE :: {re_pattern, _, _, _, _} | iodata() | unicode:charlist().

're:run'(_, _) ->
    error(eqwalizer_specs).

-spec 're:run'(Subject, RE, Options) ->
    {match, dynamic()} | match | nomatch | {error, dynamic()}
when
    Subject :: iodata() | unicode:charlist(),
    RE :: {re_pattern, _, _, _, _} | iodata() | unicode:charlist(),
    Options :: [Option],
    Option ::
        anchored
        | global
        | notbol
        | noteol
        | notempty
        | notempty_atstart
        | report_errors
        | {offset, non_neg_integer()}
        | {match_limit, non_neg_integer()}
        | {match_limit_recursion, non_neg_integer()}
        | {newline, NLSpec}
        | bsr_anycrlf
        | bsr_unicode
        | {capture, ValueSpec}
        | {capture, ValueSpec, Type}
        | CompileOpt,
    Type :: index | list | binary,
    ValueSpec :: all | all_but_first | all_names | first | none | ValueList,
    ValueList :: [ValueID],
    ValueID :: integer() | string() | atom(),
    CompileOpt ::
        unicode
        | anchored
        | caseless
        | dollar_endonly
        | dotall
        | extended
        | firstline
        | multiline
        | no_auto_capture
        | dupnames
        | ungreedy
        | {newline, NLSpec}
        | bsr_anycrlf
        | bsr_unicode
        | no_start_optimize
        | ucp
        | never_utf,
    NLSpec :: cr | crlf | lf | anycrlf | any.

're:run'(_, _, _) ->
    error(eqwalizer_specs).

%% -------- rpc --------

-spec 'rpc:call'(Node, Module, Function, Args) -> Res | {badrpc, Reason} when
    Node :: node(),
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Res :: dynamic(),
    Reason :: term().
'rpc:call'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'rpc:call'(Node, Module, Function, Args, Timeout) -> Res | {badrpc, Reason} when
    Node :: node(),
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Res :: dynamic(),
    Reason :: term(),
    Timeout :: timeout().
'rpc:call'(_, _, _, _, _) -> error(eqwalizer_specs).

%% -------- sets --------

-spec 'sets:new'() -> sets:set(none()).
'sets:new'() -> error(eqwalizer_specs).

-spec 'sets:new'(Opts :: [{version, 1..2}]) -> sets:set(none()).
'sets:new'(_) -> error(eqwalizer_specs).

%% -------- socket --------

-spec 'socket:open'(term(), term()) -> dynamic().
'socket:open'(_, _) -> error(eqwalizer_specs).

-spec 'socket:open'(socket:domain() | integer(), socket:type() | integer(), dynamic()) ->
    {ok, socket:socket()} | {error, term()}.
'socket:open'(_, _, _) -> error(eqwalizer_specs).

-spec 'socket:recv'(socket:socket()) -> dynamic().
'socket:recv'(_) -> error(eqwalizer_specs).

-spec 'socket:recv'(socket:socket(), dynamic()) -> dynamic().
'socket:recv'(_, _) -> error(eqwalizer_specs).

-spec 'socket:recv'(socket:socket(), dynamic(), dynamic()) -> dynamic().
'socket:recv'(_, _, _) -> error(eqwalizer_specs).

-spec 'socket:send'(socket:socket(), iodata()) -> dynamic().
'socket:send'(_, _) -> error(eqwalizer_specs).

-spec 'socket:send'(socket:socket(), iodata(), dynamic()) -> dynamic().
'socket:send'(_, _, _) -> error(eqwalizer_specs).

%% -------- ssl --------

-spec 'ssl:connect'(ssl:sslsocket() | ssl:host(), dynamic(), dynamic()) -> dynamic().
'ssl:connect'(_, _, _) -> error(eqwalizer_specs).

%% -------- string --------

-spec 'string:lexemes'
    (string(), [string:grapheme_cluster()]) -> [string()];
    (unicode:unicode_binary(), [string:grapheme_cluster()]) -> [unicode:unicode_binary()].
'string:lexemes'(_, _) -> error(eqwalizer_specs).

-spec 'string:lowercase'
    (string()) -> string();
    (unicode:unicode_binary()) -> unicode:unicode_binary().
'string:lowercase'(_) -> error(eqwalizer_specs).

-spec 'string:slice'
    (string(), non_neg_integer()) -> string();
    (unicode:unicode_binary(), non_neg_integer()) -> unicode:unicode_binary().
'string:slice'(_, _) -> error(eqwalizer_specs).

-spec 'string:slice'
    (string(), non_neg_integer(), 'infinity' | non_neg_integer()) -> string();
    (unicode:unicode_binary(), non_neg_integer(), 'infinity' | non_neg_integer()) ->
        unicode:unicode_binary().
'string:slice'(_, _, _) -> error(eqwalizer_specs).

-spec 'string:replace'
    (string(), string(), string()) -> [string()];
    (unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary()) ->
        [unicode:unicode_binary()].
'string:replace'(_, _, _) -> error(eqwalizer_specs).

-spec 'string:replace'
    (string(), string(), string(), leading | trailing | all) -> [string()];
    (
        unicode:unicode_binary(),
        unicode:unicode_binary(),
        unicode:unicode_binary(),
        leading | trailing | all
    ) ->
        [unicode:unicode_binary()].
'string:replace'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'string:split'
    (string(), string()) -> [string()];
    (unicode:unicode_binary(), unicode:unicode_binary()) -> [unicode:unicode_binary()].
'string:split'(_, _) -> error(eqwalizer_specs).

-spec 'string:split'
    (string(), string(), 'leading' | 'trailing' | 'all') -> [string()];
    (unicode:unicode_binary(), unicode:unicode_binary(), 'leading' | 'trailing' | 'all') ->
        [unicode:unicode_binary()].
'string:split'(_, _, _) -> error(eqwalizer_specs).

-spec 'string:trim'
    (string()) -> string();
    (unicode:unicode_binary()) -> unicode:unicode_binary().
'string:trim'(_) -> error(eqwalizer_specs).

-spec 'string:trim'
    (string(), 'leading' | 'trailing' | 'both') -> string();
    (unicode:unicode_binary(), 'leading' | 'trailing' | 'both') -> unicode:unicode_binary().
'string:trim'(_, _) -> error(eqwalizer_specs).

-spec 'string:trim'
    (string(), 'leading' | 'trailing' | 'both', [string:grapheme_cluster()]) -> string();
    (unicode:unicode_binary(), 'leading' | 'trailing' | 'both', [string:grapheme_cluster()]) ->
        unicode:unicode_binary().
'string:trim'(_, _, _) -> error(eqwalizer_specs).

-spec 'string:uppercase'
    (string()) -> string();
    (unicode:unicode_binary()) -> unicode:unicode_binary().
'string:uppercase'(_) -> error(eqwalizer_specs).

-spec 'string:chomp'(unicode:chardata()) -> eqwalizer:dynamic(unicode:chardata()).
'string:chomp'(_) -> error(eqwalizer_specs).

-spec 'string:find'(unicode:chardata(), unicode:chardata()) -> eqwalizer:dynamic(unicode:chardata()) | nomatch.
'string:find'(_, _) -> error(eqwalizer_specs).

-spec 'string:titlecase'(unicode:chardata()) -> eqwalizer:dynamic(unicode:chardata()).
'string:titlecase'(_) -> error(eqwalizer_specs).

%% -------- sys --------

-spec 'sys:get_status'(Name) -> Status when
    Name :: pid() | atom() | {'global', term()} | {'via', module(), term()},
    Status :: {status, Pid :: pid(), {module, Module :: module()}, [SItem]},
    SItem :: dynamic().
'sys:get_status'(_) -> error(eqwalizer_specs).

-spec 'sys:get_state'(Name) -> State when
    Name :: pid() | atom() | {atom(), term()} | {'via', module(), term()},
    State :: dynamic().
'sys:get_state'(_) -> error(eqwalizer_specs).

-spec 'sys:replace_state'(Name, Function) -> State when
    Name :: pid() | atom() | {atom(), term()} | {'via', module(), term()},
    Function :: function(),
    State :: dynamic().
'sys:replace_state'(_, _) -> error(eqwalizer_specs).

-spec 'sys:replace_state'(Name, Function, Timeout) -> State when
    Name :: pid() | atom() | {atom(), term()} | {'via', module(), term()},
    Function :: function(),
    Timeout :: timeout(),
    State :: dynamic().
'sys:replace_state'(_, _, _) -> error(eqwalizer_specs).

%% -------- test_server --------

-spec 'test_server:start_peer'(
    [string()] | peer:start_options() | #{start_cover => boolean()},
    atom() | string(),
    TestCase :: atom() | string()
) ->
    {ok, peer:server_ref(), node()} | {error, term()}.
'test_server:start_peer'(_, _, _) -> error(eqwalizer_specs).

%% -------- timer --------

-spec 'timer:tc'(fun(() -> T)) -> {integer(), T}.
'timer:tc'(_) -> error(eqwalizer_specs).

-spec 'timer:tc'(Fun, ArgumentsOrUnit) -> {Time, Value} when
    Fun :: function(),
    ArgumentsOrUnit :: [term()] | erlang:time_unit(),
    Time :: integer(),
    Value :: dynamic().
'timer:tc'(_, _) -> error(eqwalizer_specs).

-spec 'timer:tc'(module(), atom(), [term()] | erlang:time_unit()) -> {integer(), dynamic()}.
'timer:tc'(_, _, _) -> error(eqwalizer_specs).

-spec 'filename:join'([file:name_all()]) -> dynamic().
'filename:join'(_) -> error(eqwalizer_specs).

-spec 'filename:join'(file:name_all(), file:name_all()) -> dynamic().
'filename:join'(_, _) -> error(eqwalizer_specs).
%% -------- xmerl --------

-spec 'xmerl:export_simple'(Content, Callback) -> io_lib:chars() when
    Content :: [Element],
    Element :: dynamic(),
    Callback :: module() | [module()].
'xmerl:export_simple'(_, _) -> error(eqwalizer_specs).

-spec 'xmerl:export_simple'(Content, Callback, RootAttributes) -> io_lib:chars() when
    Content :: [Element],
    Element :: dynamic(),
    Callback :: module() | [module()],
    RootAttributes :: [dynamic()].
'xmerl:export_simple'(_, _, _) -> error(eqwalizer_specs).

%% -------- xmerl_xpath --------

-spec 'xmerl_xpath:string'(dynamic(), dynamic()) -> dynamic().
'xmerl_xpath:string'(_, _) -> error(eqwalizer_specs).
