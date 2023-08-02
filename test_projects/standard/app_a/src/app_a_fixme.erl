-module(app_a_fixme).
-export([notok/0]).
-eqwalizer(fixme).

-spec notok() -> ok.
notok() -> err.
