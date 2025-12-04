-module(app_b).

-define(MACRO_A, 1).
-define(MACRO_B, 1).

main() ->
    ?MACRO_A.
