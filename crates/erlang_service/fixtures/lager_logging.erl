-module(lager_logging).
-compile({parse_transform, lager_transform}).
-export([f/0]).

f() ->
    lager:info("x").
