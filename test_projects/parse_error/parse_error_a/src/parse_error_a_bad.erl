-module(parse_error_a_bad).
-compile([export_all, nowarn_export_all]).
-typing([eqwalizer]).

foo() -> ok;
foon() -> ok. % head-mismatch
