-module(suppressed).

-export([do/0]).

-spec do() -> ok.
do() ->
    % elp:fixme W0007 - This is a test
    Life = 42,
    ok.
