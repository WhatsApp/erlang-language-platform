-module(compiler_diagnostics).
-compile(warn_missing_spec).

-behaviour(application).

-export([foo/0]).
-spec foo() -> ok.
foo() ->
  green.
