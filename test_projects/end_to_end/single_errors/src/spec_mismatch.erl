% To reproduce T106706879.
-module(spec_mismatch).
-compile(warn_missing_spec).
-typing([eqwalizer]).
-export([foo/0]).
-spec foo() -> ok.
foo() ->
    green.
