-module(ct_info_dynamic_SUITE).

-export([all/0, groups/0]).
-export([a/1, b/1, c/1]).

all() -> all_helper().

all_helper() -> [a, {group, group_a}].

groups() -> groups_helper().

groups_helper() -> [{group_a, [b, c]}].

a(_Config) ->
    ok.

b(_Config) ->
    ok.

c(_Config) ->
    ok.
