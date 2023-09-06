-module(app_a).
-export([application_env_error_app_a/0]).

application_env_error_app_a() ->
    application:get_env(misc, key).

food(0) ->
    ok;
fooX(X) ->
    no.
