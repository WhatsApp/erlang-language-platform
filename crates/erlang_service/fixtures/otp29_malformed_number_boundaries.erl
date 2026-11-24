-module(otp29_malformed_number_boundaries).
-export([f/0, g/0, h/0, i/0, j/0, k/0, l/0]).

f() -> {"é", 123abc}.
g() -> {"é", 123.foo}.
h() -> {"é", 16#ffg}.
i() -> {"é", 16#f.fg}.
j() -> {"é", 1.5e2units}.
k() -> {"é", 1.5units}.
l() -> {"é", 16#ff.g}.
