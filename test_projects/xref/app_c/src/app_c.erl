-module(app_c).
-export([my_function/0]).

-type my_type_c() :: integer().
-export_type([my_type_c/0]).

-spec my_function() -> my_type_c().
my_function() -> 42.
