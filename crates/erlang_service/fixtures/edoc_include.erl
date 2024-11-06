%% @doc This module includes a header file
-module(edoc_include).

-include_lib("kernel/include/logger.hrl").

%% @doc This is function 1
one() ->
    1.

%% @docc This is function two, containing a warning
two() ->
    2.
