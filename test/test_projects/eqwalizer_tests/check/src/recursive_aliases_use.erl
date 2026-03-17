-module(recursive_aliases_use).
-compile([export_all, nowarn_export_all]).

-spec test1(recursive_aliases:invalid_rec()) -> ok.
test1(_) -> ok.

-spec test2(recursive_aliases:invalid_transitive()) -> ok.
test2(_) -> ok.
