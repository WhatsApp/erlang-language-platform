-module(type_error).
-export([g/0]).

-spec g() -> integer().
g() -> not_an_integer.
