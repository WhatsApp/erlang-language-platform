-module(custom_function_matches).
-moduledoc """
Tests for custom function matches.
""".
-compile(warn_missing_spec_all).
-export([main/0]).

-spec main() -> ok.
-doc """
Main function.
""".
main() ->
    excluded:function(),
    not_excluded:function(),
    cross:call(),
    ok.
