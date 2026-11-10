-module(app_a_test_helpers).
-export([skipped/0]).

-spec skipped() -> ok.
skipped() -> error.
