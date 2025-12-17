-module(app_a).

-define(MACRO_A, 1).
-define(MACRO_B, 1).

main() ->
    ?MACRO_A.
