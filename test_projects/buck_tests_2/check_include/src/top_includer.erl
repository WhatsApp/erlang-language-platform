-module(top_includer).

-compile(warn_missing_spec_all).

-include_lib("check_include_separate_1/include/top_includer.hrl").

-define(A_MACRO, ?FUNCTION_NAME).

foo() ->
    ?FIRST,
    ?A_MACRO
    ?SECOND,
    ?THIRD(41,34),
    ?FUNCTION_NAME.
