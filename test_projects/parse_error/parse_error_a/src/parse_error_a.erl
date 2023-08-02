-module(parse_error_a).
-export([
    test1/1, test2_neg/1



]).

-spec test1(_) -> atom().
test1(X) when is_atom(X) ->
    X.

-spec test2_neg([number()]) -> atom().
test2_neg(X) ->
    X.
