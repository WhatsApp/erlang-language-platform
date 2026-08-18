-module(app_b).
-include("shared.hrl").
-export([g/0]).

-spec g() -> app_b_type().
g() ->
    ok.
