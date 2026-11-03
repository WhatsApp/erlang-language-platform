-module(fix_selection).

-export([positive_integer/1]).

positive_integer(X) ->
    is_integer(X) andalso X > 0.
