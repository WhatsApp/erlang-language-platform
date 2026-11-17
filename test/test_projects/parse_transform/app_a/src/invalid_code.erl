-module(invalid_code).
-compile({parse_transform, fake_lager_transform}).
-export([f/0]).

-spec f() -> not_ok.
f() ->
    fake_lager:info("x"),
    ok.
