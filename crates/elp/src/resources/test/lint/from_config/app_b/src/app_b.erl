-module(app_b).
-export([application_env_error/0, application_env_no_error/0]).

application_env_error() ->
    ok.

application_env_no_error() ->
    ok.
