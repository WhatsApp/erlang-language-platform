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

-spec no_map_rewrite(#{undefined | binary() => atom()}) -> #{binary() => atom()}.
no_map_rewrite(M) -> M.

-record(foo, {bar :: atom(), baz :: atom()}).

-spec no_record_conversion_1(#foo{}) -> {foobar, atom()}.
no_record_conversion_1(Foo) -> Foo.

-spec no_record_conversion_2(#foo{}) -> {binary(), atom(), atom()}.
no_record_conversion_2(Foo) -> Foo.

-spec record_conversion(#foo{}) -> {foo, binary(), atom()}.
record_conversion(Foo) -> Foo.

-spec no_good_map_candidate_1(#{foo => #{large_map_key_a => large_map_val_a, large_map_key_b => large_map_val_b}})
    -> #{foo => #{large_map_key_c => large_map_val_c} | #{large_map_key_d => large_map_val_d}}.
no_good_map_candidate_1(M) -> M.

-spec no_good_map_candidate_2(#{foo => #{large_map_key_a => large_map_val_a, large_map_key_b => large_map_val_b}})
    -> #{foo => #{large_map_key_c => large_map_val_c, large_map_key_d => large_map_val_d} | any}.
no_good_map_candidate_2(M) -> M.
