-module(otp29_valid_inline).
-export([f/0]).
-compile({inline, [f/0]}).

f() -> ok.
