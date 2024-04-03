-module(wa_utils).

-export([init/1]).

init(State0) ->
    Providers = [wa_build_info_prv],
    State1 = lists:foldl(
        fun(Provider, State) ->
            {ok, NewState} = Provider:init(State),
            NewState
        end,
        State0,
        Providers
    ),
    {ok, State1}.
