-module(app_b).
-export([application_env_error/0, application_env_no_error/0]).

application_env_error() ->
    % elp:ignore W0011 (application_get_env)
    application:get_env(misc, key).

application_env_no_error() ->
    application:get_env(app_b, key).
