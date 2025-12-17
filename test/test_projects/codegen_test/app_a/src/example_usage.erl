-module(example_usage).
-compile([warn_missing_spec_all]).
-moduledoc """
Example module that demonstrates using generated code.
This module uses the types and client functions generated from
the example_service.schema file.
""".

%% Include the generated header file
-include_lib("example_service_generated/include/example_service_types.hrl").

%% API exports
-export([
    create_sample_user/0,
    create_query_request/1,
    get_user_by_id/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-doc """
Creates a sample user_info record using generated types
""".
-spec create_sample_user() -> #user_info{}.
create_sample_user() ->
    #user_info{
        user_id = <<"user_123">>,
        username = <<"john_doe">>,
        age = 25,
        is_active = true
    }.

-doc """
Creates a query_request record
""".
-spec create_query_request(binary()) -> #query_request{}.
create_query_request(QueryId) ->
    #query_request{
        query_id = QueryId,
        filters = [{age, greater_than, 18}]
    }.

-doc """
Uses the generated client to get user information
""".
-spec get_user_by_id(binary()) -> {ok, term()} | {error, term()}.
get_user_by_id(UserId) ->
    %% This calls the generated client function
    example_service_client:get_user(UserId).
