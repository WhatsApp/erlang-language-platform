-module(lint_recursive).

-export([test_foo/1]).

do_something() ->
    ok,
    ok.

test_foo(Config) ->
    do_something(),
    Config1 = Config,
    clean_mocks().

clean_mocks() ->
    redundant,
    ok.
