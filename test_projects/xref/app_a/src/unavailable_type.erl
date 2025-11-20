-module(unavailable_type).
-export([test_types/0]).

-spec test_types() -> {app_b:my_type_b(), app_c:my_type_c()}.
test_types() ->
    {"hello", 42}.
