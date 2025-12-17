-module(app_a_unreachable_test_SUITE).
-export([all/0, a/1, b/1]).

all() -> [a].

a(_) -> ok.

b(_) -> ok.
