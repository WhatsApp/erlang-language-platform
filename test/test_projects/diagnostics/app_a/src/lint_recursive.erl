-module(lint_recursive).

-export([test_foo/1, test_foo2/1]).

do_something() ->
    ok,
    ok.

%% We want to check that the "no effect" statements in test_foo/1 and
%% test_foo2/1 are removed, but not the ones in clean_mocks/0 and
%% something/0.
test_foo(Config) ->
    do_something(),
    Config1 = Config,
    clean_mocks().

test_foo2(Config) ->
    do_something(),
    Config1 = Config,
    clean_mocks().

clean_mocks() ->
    redundant,
    ok.
