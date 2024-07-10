%% % @format
-module(expression_updates_literal).
-compile(warn_missing_spec_all).

-export([a_fun/0]).

a_fun() ->
    [ #{a => 1}
      #{a => 2}
    ].
