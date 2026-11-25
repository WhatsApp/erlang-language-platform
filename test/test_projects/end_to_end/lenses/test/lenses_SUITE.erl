-module(lenses_SUITE).
-export([all/0, main/1]).

all() -> [main].

main(_Config) ->
    Value = value(),
    value = Value.

value() ->
    value.
