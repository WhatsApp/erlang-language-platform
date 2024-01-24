-module(override_warn_missing_spec_all).
-compile(nowarn_missing_spec_all).

-export([main/0]).

-spec main() -> ok.
main() -> not_exported().

not_exported() -> ok.
