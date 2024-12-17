%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service_ct).

-export([run/2]).

run(_Id, [Module, Filename, CompileOptions, ShouldRequestGroups, AstBinary]) ->
    {ok, Forms0, _} = binary_to_term(AstBinary),
    Forms1 = elp_parse:map_anno(fun(_) -> 0 end, Forms0),
    {ok, Module, Binary}
        = case compile:noenv_forms(Forms1, CompileOptions) of
            {ok, M, B, _} -> {ok, M, B};
            {ok, M, B} -> {ok, M, B}
          end,
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
