-module(app_a_two_errors).
-export([one/0, two/0]).

-spec one() -> ok.
one() -> error.

-spec two() -> ok.
two() -> error.
