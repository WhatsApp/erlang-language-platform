-module(app_b).
-include_lib("app_a/include/app_a.hrl").

-spec test_fun() -> ok.
test_fun() -> type_error.
