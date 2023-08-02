%%% % @format
-module(test_elp_direct_dep).
-oncall("whatsapp_dev_infra").
-typing([eqwalizer]).

-include("test_elp_direct_dep.hrl").
-include("test_elp_direct_dep_private.hrl").
-include_lib("test_elp/include/test_elp.hrl").
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
