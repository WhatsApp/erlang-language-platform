-module(otp29_union_singleton_typevars).
-export([f/0, g/1, h/0]).
-compile(nowarn_singleton_typevar).

-spec f() -> {ok, A} | {error, A}.
f() -> {ok, 1}.

-spec g(B) -> {ok, B} | {error, B}.
g(B) -> {ok, B}.

-spec h() -> {ok, _C} | {error, _C}.
h() -> {ok, 1}.
