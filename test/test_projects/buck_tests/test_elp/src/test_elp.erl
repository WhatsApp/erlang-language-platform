%% @doc some module
-module(test_elp).
-oncall("whatsapp_dev_infra").
-typing([eqwalizer]).

-include("test_elp.hrl").
-include_lib("test_elp_transitive_dep/include/test_elp_transitive_dep.hrl").
-include_lib("test_elp_direct_dep/include/test_elp_direct_dep.hrl").
-include_lib("test_elp_no_private_headers/include/test_elp_no_private_headers.hrl").
-include_lib("kernel/include/logger.hrl").

-export([mode/0, main/1, crash/0]).

%% The spec within -ifdef() is here to test that edoc preprocessing works
%% (generating edoc for this file without preprocessing will fail)
-ifdef(TEST).
mode() -> dev.
-else.
mode() -> prod.
-endif.


%% @doc some function
%% @see crash/0
-spec main([string()]) -> no_return().
main(ArgV) ->
    io:format("Mode: ~p~n", [mode()]),
    io:format("ArgV: ~p~n", [ArgV]),
    erlang:halt(0).

crash() ->
    io:format("?FILE: ~s~n", [?FILE]),
    error(crash).
