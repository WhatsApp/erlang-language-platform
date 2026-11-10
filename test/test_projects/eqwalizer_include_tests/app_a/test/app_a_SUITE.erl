-module(app_a_SUITE).
-export([skipped/0]).

-spec skipped() -> ok.
skipped() -> error.
