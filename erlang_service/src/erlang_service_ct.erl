-module(erlang_service_ct).

-export([run/1]).

run([Module, Filename, CompileOptions, ShouldRequestGroups]) ->
    {ok, Module, Binary} = compile:file(Filename, [binary|normalize_compile_options(CompileOptions)]),
    code:load_binary(Module, Filename, Binary),
    All = eval(lists:flatten(io_lib:format("~p:all().", [Module]))),
    Groups = case ShouldRequestGroups of
        true ->
            eval(lists:flatten(io_lib:format("~p:groups().", [Module])));
        false ->
            []
    end,
    code:delete(Module),
    code:purge(Module),
    {ok, [{"CT_INFO_ALL", term_to_binary(All)}, {"CT_INFO_GROUPS", term_to_binary(Groups)}]}.

eval(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, []),
    Value.

normalize_compile_options(CompileOptions) ->
    normalize_compile_options(CompileOptions, []).

normalize_compile_options([], Acc) ->
    Acc;
normalize_compile_options([{includes, Includes} | Tail], Acc) ->
    normalize_compile_options(Tail, Acc ++ [{i, Include} || Include <- Includes]);
normalize_compile_options([Head | Tail], Acc) ->
    normalize_compile_options(Tail, [Head | Acc]).
