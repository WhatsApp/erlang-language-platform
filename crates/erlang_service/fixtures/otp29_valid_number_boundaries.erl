-module(otp29_valid_number_boundaries).
-export([f/0]).

f() -> {123, 16#ff, 2#10.1#e2, 1.5e2}.
