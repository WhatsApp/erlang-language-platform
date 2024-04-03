-module(app_b).
-typing([eqwalizer]).

-export([foo/0]).

-spec foo() -> binary().
foo() ->
    app_a:foo().
