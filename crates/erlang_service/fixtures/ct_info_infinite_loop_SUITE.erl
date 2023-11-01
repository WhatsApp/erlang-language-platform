-module(ct_info_infinite_loop_SUITE).

-export([all/0, groups/0]).
-export([a/1, b/1, c/1]).

all() -> [a, b, c].

groups() -> groups().

a(_Config) ->
    ok.

b(_Config) ->
    ok.

c(_Config) ->
    ok.
