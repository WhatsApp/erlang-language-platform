-module(dyn_pattern).
-export([f/1, g/2, h/1, l/0, nested/1, expr/0, remote/0]).
-define(dyn_pattern, :?).

f(X = ?dyn_pattern) -> X.

g({A, B} = ?dyn_pattern, C = D = :?) -> {A, B, C, D}.

h(X) ->
    case X of
        Y = ?dyn_pattern -> Y
    end.

l() -> fun(X = :?) -> X end.

nested({X = :?}) -> X.

expr() -> :?.

-define(F, f).
remote() -> ?MODULE:?F(1).
