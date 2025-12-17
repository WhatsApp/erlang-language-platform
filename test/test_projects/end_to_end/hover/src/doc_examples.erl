%% @doc Dummy module for hover tests.
-module(doc_examples).
-compile(nowarn_missing_spec).
-export([f/1,g/1]).


%% @doc
%% Multi-lines example.
%% Only for test purpose.
%% @end
-spec f([T]) -> [T].
f(L) ->
    ListsMod = string,
    code:ensure_loaded(ListsMod), % A reference to the `lists` OTP module
    % We'll check we get the doc for reverse/2.
    lists:reverse(L).

g(L) ->
    % We'll check we get the doc for f/1.
    f(L) ++ f(L).

-define(FOO(A), A+1).
h() -> ?FOO(3).
