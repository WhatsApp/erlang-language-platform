-module(app_a).

-export([foo/0]).

-include_lib("auto_gen_a/include/auto_gen_a.hrl").
-include("generated_header.hrl").
% -include_lib("generated_headers/include/generated_header.hrl").
% -include_lib("buck_tests_2/generated/generated_header.hrl").

foo() -> {?hello, ?AG}.
