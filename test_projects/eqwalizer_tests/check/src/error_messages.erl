-module(error_messages).

-compile([export_all, nowarn_export_all]).

-type foo_map() :: #{
    bar => atom(),
    baz => integer(),
    other => atom(),
    other2 => atom(),
    other3 => atom(),
    other4 => atom(),
    other5 => atom()}.

-spec map_candidates(#{bar := atom(), baz := atom()}) ->
    foo_map() | #{foo => atom()}.
map_candidates(M) -> M.

-spec map_candidates_2([#{bar => a | b} | #{baz => a | b}]) -> [#{bar => a}].
map_candidates_2(M) -> M.
