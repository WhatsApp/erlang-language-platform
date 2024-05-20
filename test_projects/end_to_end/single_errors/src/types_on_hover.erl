-module(types_on_hover).
-compile(warn_missing_spec).
-typing([eqwalizer]).
-behaviour(application).

-export([foo/1]).
-spec foo(my_type) -> my_type.
foo(Value) -> Value.
