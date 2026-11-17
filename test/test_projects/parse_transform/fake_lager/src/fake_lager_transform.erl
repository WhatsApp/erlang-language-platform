%% Mimics lager's `lager_transform` (lager 3.9.2). It rewrites
%% `fake_lager:info(Message)` into `fake_lager:log(Line, Message)` and, like
%% the real transform, builds the line literal as `{integer, Anno, Anno}`: with
%% column-aware annotations the literal's value is a `{Line, Column}` tuple
%% rather than an integer.
-module(fake_lager_transform).
-export([parse_transform/2]).

-spec parse_transform([erl_parse:abstract_form()], [compile:option()]) ->
    [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
    transform(Forms).

transform({call, Anno, {remote, _, {atom, _, fake_lager}, {atom, _, info}}, [Message]}) ->
    {call, Anno, {remote, Anno, {atom, Anno, fake_lager}, {atom, Anno, log}}, [
        {integer, Anno, Anno}, transform(Message)
    ]};
transform(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(transform(tuple_to_list(Tuple)));
transform(List) when is_list(List) ->
    [transform(Elem) || Elem <- List];
transform(Other) ->
    Other.
