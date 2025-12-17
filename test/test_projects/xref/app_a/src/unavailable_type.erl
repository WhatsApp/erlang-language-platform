-module(unavailable_type).
-export([test_types/0]).

-record(my_record, {
    field_a :: app_b:my_type_b(),
    field_b :: app_c:my_type_c(),
    field_c :: binary()
}).

-spec test_types() -> {app_b:my_type_b(), app_c:my_type_c(), #my_record{}}.
test_types() ->
    {"hello", 42, #my_record{field_a = "", field_b = 42, field_c = <<>>}}.
