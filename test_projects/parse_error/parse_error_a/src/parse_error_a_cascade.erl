-module(parse_error_a_cascade).

-export([foo/0]).

foo() ->
     bar().

bar() ->

    %% syntax error
    .
