-module(app_a).

-include("app_a.hrl").
-include_lib("inets/include/httpd.hrl").

-import(foo, [bar/1]).
-export([ok/0, local_call/2, f/2]).
-export_type([type_a/0]).

-behaviour(gen_server).

-callback is_enabled(any()) -> boolean().

-spec ok() -> ok.
ok() -> ok.

-spec local_call(integer(), any()) -> tuple();
                (float(), any()) -> tuple().
local_call(1, B) ->
  {1, B};
local_call(C, D) ->
  local_call(1,C+D).

f(Mod, Foo) ->
  ok(),
  (ok)(),
  foo:ok(),
  (mod:foo)(),
  (Mod):(Foo)().

-define(FOO(A), A+1).

-type type_a() :: any().

g(A) ->
  F = fun local_call/2,
  G = fun mod_name:fun_name/2,
  H = fun (X) -> X + 1 end,
  I = fun A(X) -> X + 1 end.

h() ->
    ?FOO(3).

-define(BAR, ok).
i() ->
    ?BAR.

-record(r1, {f1 :: integer(), f2}).

j(R, #r1{f1 = A, f2 = B} = S) ->
  #r1.f1 = R,
  #r1{f1 = R, f2 = ok},
  S#r1{f1 = 3} .

-opaque session(B) :: #{uri := uri(), b := B}.
-type source(A) :: binary(A).

map(blah, <<"release">>) -> ?BAR.

with_dot() -> 'ab.cd'.

lang_dir('div') -> rtl; % Divehi
lang_dir('baz') -> ltr;
lang_dir(_Foo) -> ltr.

escape([$\" | T]) -> ok.
-record(all_configs_file, {
    'a/b/c' :: binary()
}).

k() ->
  (foo:bar(1, 2))#record{ f1 = 4}.

l(A) ->
    A#record.f1.

m() ->
  fun
    ('div') -> rtl; % Divehi
    ('baz') -> ltr;
    (_Foo) -> ltr
  end.
