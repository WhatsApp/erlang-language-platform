-module(app_a_mod2).
-include("app_a.hrl").
-typing([eqwalizer]).
-export([
    id/1, unspecced/0, unspecced2_neg/1






]).
-export_type([
    alias/1,
    invalid/0



]).

-type alias(T) :: app_b:tup(T).
-type invalid() :: invalid().

-spec id(X) -> X.
id(X) ->
    X.

unspecced() -> whatevs.

unspecced2_neg(X) ->
    1 + an_atom,
    X.
