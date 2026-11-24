-module(function_arity).
-export([zero/0, list/1, tuple/1, binary/1, nested/1, multiple/2]).

zero() -> ?FUNCTION_ARITY.
list([]) -> ?FUNCTION_ARITY.
tuple({}) -> ?FUNCTION_ARITY.
binary(<<>>) -> ?FUNCTION_ARITY.
nested([[], {}]) -> ?FUNCTION_ARITY.

multiple([], 0) -> ?FUNCTION_ARITY;
multiple(_, _) -> ?FUNCTION_ARITY.
