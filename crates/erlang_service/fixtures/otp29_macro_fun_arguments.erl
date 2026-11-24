-module(otp29_macro_fun_arguments).
-export([f/0]).
-export_type([t/0]).

-define(ID(T), T).
-define(PAIR(T, U), {T, U}).

-type t() :: ?PAIR(fun(() -> ok), fun((fun(() -> ok)) -> ok)).

f() ->
    {?ID(fun() -> ok end),
     ?ID(fun(X) when is_atom(X), X =/= bad -> X end),
     ?ID(fun Named(0) -> ok; Named(N) -> Named(N - 1) end)}.
