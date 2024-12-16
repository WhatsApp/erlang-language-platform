%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service_ct).

-export([run/2]).

run(_Id, [Module, Filename, CompileOptions, ShouldRequestGroups]) ->
    {ok, Module, Binary} = compile:file(Filename, [binary | normalize_compile_options(CompileOptions)]),
    code:load_binary(Module, Filename, Binary),
    {All, Groups} =
        try
            case ShouldRequestGroups of
                true ->
                    {Module:all(), Module:groups()};
                false ->
                    {Module:all(), []}
            end
        after
            code:purge(Module),
            code:delete(Module)
        end,
    {ok, [{<<"ALL">>, term_to_binary(All)}, {<<"GRP">>, term_to_binary(Groups)}]}.

normalize_compile_options(CompileOptions) ->
    normalize_compile_options(CompileOptions, []).

normalize_compile_options([], Acc) ->
    Acc;
normalize_compile_options([{includes, Includes} | Tail], Acc) ->
    normalize_compile_options(Tail, Acc ++ [{i, Include} || Include <- Includes]);
normalize_compile_options([Head | Tail], Acc) ->
    normalize_compile_options(Tail, [Head | Acc]).
