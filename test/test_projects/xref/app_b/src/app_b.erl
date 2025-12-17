-module(app_b).
-export([my_function/0]).

-type my_type_b() :: string().
-export_type([my_type_b/0]).

-spec my_function() -> my_type_b().
my_function() -> "hello from b".
