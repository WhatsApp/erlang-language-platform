-module(lint_recursive).

-export([test_foo/1]).

do_something() ->
    ok,
    ok.

test_foo(_Config) ->
    do_something(),
    clean_mocks().

clean_mocks() ->
    redundant,
    ok.
