-module(unavailable_function).
-export([test_calls/0]).

-spec test_calls() -> {string(), integer()}.
test_calls() ->
    {app_b:my_function(), app_c:my_function()}.
