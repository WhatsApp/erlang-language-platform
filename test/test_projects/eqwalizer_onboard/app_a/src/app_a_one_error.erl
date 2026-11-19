-module(app_a_one_error).
-export([one/0]).

-spec one() -> ok.
one() -> error.
