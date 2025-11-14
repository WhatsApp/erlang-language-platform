-module(top_includer).

-compile(warn_missing_spec_all).

-export([foo/0]).

-include_lib("check_include_separate_1/include/top_includer.hrl").

-spec foo() -> ok.
foo() ->
    ?FIRST,
    ?SECOND,
    ?THIRD(41,34),
    ?FUNCTION_NAME.
