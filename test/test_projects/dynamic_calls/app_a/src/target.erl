-module(target).
-export([builtin_target/1, custom_target/1, unused/0]).

builtin_target(X) -> X + 1.

custom_target(X) -> X * 2.

unused() -> ok.
