-module(app_a_ignored).
-export([notok/0]).
-eqwalizer(ignore).

-spec notok() -> ok.
notok() -> err.
