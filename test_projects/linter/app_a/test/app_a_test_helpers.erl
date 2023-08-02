-module(app_a_test_helpers).
-typing([eqwalizer]).
-compile([export_all, nowarn_export_all]).

-spec fail() -> error.
fail() -> wrong_ret.

-spec ok() -> ok.
ok() -> ok.

