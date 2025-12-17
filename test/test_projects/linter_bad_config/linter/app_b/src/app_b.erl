-module(app_b).
-export([application_env_error/0]).

application_env_error() ->
    application:get_env(misc, key).
