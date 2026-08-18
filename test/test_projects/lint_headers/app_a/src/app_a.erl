-module(app_a).
-include("shared.hrl").
-export([f/0]).

-spec f() -> app_a_type().
f() ->
    ok.
