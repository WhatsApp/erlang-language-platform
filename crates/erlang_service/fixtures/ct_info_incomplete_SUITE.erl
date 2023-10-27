-module(ct_info_incomplete_SUITE).

-export([all/0, groups/0]).
-export([a/1, b/1, c/1]).

all() -> incomplete

groups() -> groups_helper().

groups_helper() -> [{group_a, [b, c]}].

a(_Config) ->
    ok.

b(_Config) ->
    ok.

c(_Config) ->
    ok.
