-module(app_a).
-export([application_env_error_app_a/0]).

application_env_error_app_a() ->
    application:get_env(misc, key).

food(0) ->
    ok;
fooX(X) ->
    no.

bar() ->
    app_a:baz(b, a),
    app_b:application_env_error().

baz(A,B) -> {A,B}.

% Some more context, see if it renders

bat(A,B) -> {A,B}.

% And some more context, see if it renders
