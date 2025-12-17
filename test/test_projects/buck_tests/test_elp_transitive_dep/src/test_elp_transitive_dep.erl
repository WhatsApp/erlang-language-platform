%%% % @format
-module(test_elp_transitive_dep).
-oncall("whatsapp_dev_infra").
-typing([eqwalizer]).

-include("test_elp_transitive_dep.hrl").
-include("test_elp_transitive_dep_private.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    io:format("Start Args: ~p~n", [StartArgs]),
    {ok,
        spawn(fun() ->
            receive
                _ -> ok
            end
        end)}.

stop(_State) ->
    ok.
