-module(common_test_SUITE).

-export([all/0, groups/0]).
-export([a/1, b/1, c/1]).

all() -> [a, {group, group_a}].

groups() ->
    [{group_a, [b, c]}].

a(_Config) ->
    ok.

b(_Config) ->
    ok.

c(_Config) ->
    ok.
