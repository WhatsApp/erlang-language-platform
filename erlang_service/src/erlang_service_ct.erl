%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
%%% % @format
-module(erlang_service_ct).

-export([run/2]).

run(_Id, [ShouldRequestGroups, AstBinary]) ->
    {ok, Forms0, _} = binary_to_term(AstBinary),
    Anno0 = erl_anno:new(0),
    Forms1 = elp_parse:map_anno(fun(_) -> Anno0 end, Forms0),
    {All, Groups} = interpret(Forms1, ShouldRequestGroups, Anno0),
    {ok, [{<<"ALL">>, term_to_binary(All)}, {<<"GRP">>, term_to_binary(Groups)}]}.

interpret(Forms, ShouldRequestGroups, Anno) ->
    Dict = parse_to_map(Forms),
    All = {call, Anno, {atom, Anno, all}, []},
    Value =
        case ShouldRequestGroups of
            true -> {tuple, Anno, [All, {call, Anno, {atom, Anno, groups}, []}]};
            false -> {tuple, Anno, [All, {nil, Anno}]}
        end,
    Handler = {value, fun(Name, Args) -> code_handler(Name, Args, Dict) end},
    {value, Result, _} = erl_eval:expr(Value, erl_eval:new_bindings(), Handler),
    Result.

parse_to_map(Forms) ->
    %% we still support OTP 25, we can't use map comprehensions yet
    maps:from_list(
        [{{local, Name, Arity}, Clauses} || {function, _, Name, Arity, Clauses} <- Forms] ++
            [{{remote, NA}, Mod} || {attribute, _, import, {Mod, NAs}} <- Forms, NA <- NAs]
    ).

code_handler(Name, Args, Dict) ->
    Arity = length(Args),
    case Dict of
        #{{local, Name, Arity} := Clauses} ->
            Handler = {value, fun(N, As) -> code_handler(N, As, Dict) end},
            case erl_eval:match_clause(Clauses, Args, erl_eval:new_bindings(), Handler) of
                {Body, Bindings} ->
                    {value, Val, _} = erl_eval:exprs(Body, Bindings, Handler),
                    Val;
                nomatch ->
                    erlang:error({function_clause, [{Name, Args}]})
            end;
        #{{remote, {Name, Arity}} := Mod} ->
            apply(Mod, Name, Args);
        _ ->
            erlang:error({undef, {Name, Arity}})
    end.
