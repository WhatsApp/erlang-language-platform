-module(my_rpc).
-export([call/4]).

%% In production this would forward to a remote node.
call(_Node, Module, Function, Args) ->
    erlang:apply(Module, Function, Args).
