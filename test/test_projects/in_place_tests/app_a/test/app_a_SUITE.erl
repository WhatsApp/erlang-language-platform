-module(app_a_SUITE).
-compile([export_all, nowarn_export_all]).
-typing([eqwalizer]).

-include("app_a.hrl").
-define(assert(X), X =:= true).

-spec ok() -> ok.
ok() ->
    ?assert(true),
    case rand:uniform(1) of
        1 -> app_a_test_helpers:ok();
        _ -> app_a_test_helpers_not_opted_in:ok()
    end.

-spec fail() -> ok.
fail() ->
    app_a_test_helpers:fail().

