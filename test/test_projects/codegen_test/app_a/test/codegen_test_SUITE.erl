%%% Test suite for code generation functionality
-module(codegen_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include("example_service_types.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    test_generated_types/1,
    test_generated_client/1,
    test_user_record_creation/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_generated_types,
        test_generated_client,
        test_user_record_creation
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_generated_types(_Config) ->
    %% Test that we can create a user_info record
    User = #user_info{
        user_id = <<"test_123">>,
        username = <<"testuser">>,
        age = 30,
        is_active = true
    },

    %% Verify the record fields
    ?assertEqual(<<"test_123">>, User#user_info.user_id),
    ?assertEqual(<<"testuser">>, User#user_info.username),
    ?assertEqual(30, User#user_info.age),
    ?assertEqual(true, User#user_info.is_active),

    ok.

test_generated_client(_Config) ->
    %% Test that the generated client module exists and can be called
    Result = example_service_client:get_user(<<"user_123">>),

    %% The generated stub returns {ok, generated_response}
    ?assertMatch({ok, _}, Result),

    ok.

test_user_record_creation(_Config) ->
    %% Test the example_usage module
    User = example_usage:create_sample_user(),

    %% Verify it's a valid user_info record
    ?assertMatch(#user_info{}, User),
    ?assertEqual(<<"user_123">>, User#user_info.user_id),
    ?assertEqual(25, User#user_info.age),

    ok.
