-module(lints).
-export([head_mismatch/1]).

head_mismatch(X) -> X;
head_mismatch(0) -> 0.

