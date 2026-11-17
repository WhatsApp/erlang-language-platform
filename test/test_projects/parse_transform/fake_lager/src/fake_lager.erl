-module(fake_lager).
-export([info/1, log/2]).

-spec info(string()) -> ok.
info(Message) ->
    log(0, Message).

-spec log(term(), string()) -> ok.
log(_Line, _Message) ->
    ok.
