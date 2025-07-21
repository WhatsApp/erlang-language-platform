-module(main_app).

%% This include_lib should fail because external_app is not a
%% dependency of main_app
-include_lib("external_app/include/external_header.hrl").

%% This include_lib should succeed, because extra_app is in
%% `extra_includes` for main_app
-include_lib("extra_app/include/extra_header.hrl").

%% Check that OTP libs are allowed
-include_lib("stdlib/include/assert.hrl").

-export([test_function/0]).

test_function() ->
    ?EXTRA_MACRO,
    %% Try to use something from the header that should not be
    %% available
    ?EXTERNAL_MACRO.
