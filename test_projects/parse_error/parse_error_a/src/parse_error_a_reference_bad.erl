-module(parse_error_a_reference_bad).
-export([main/0]).

-spec main() -> atom().
main() ->
    parse_error_a_bad:foo().
