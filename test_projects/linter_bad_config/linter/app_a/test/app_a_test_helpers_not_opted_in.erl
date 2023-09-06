-module(app_a_test_helpers_not_opted_in).
-compile([export_all, nowarn_export_all]).

-spec fail() -> ok.
fail() -> error.

-spec ok() -> ok.
ok() -> ok.

