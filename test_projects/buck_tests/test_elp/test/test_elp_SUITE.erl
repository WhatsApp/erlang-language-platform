-module(test_elp_SUITE).
-oncall("whatsapp_dev_infra").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("test_elp_direct_dep/include/test_elp_direct_dep.hrl").

-include("test_elp.hrl").
-include_lib("test_elp_transitive_dep/include/test_elp_transitive_dep.hrl").
-include_lib("test_elp_transitive_dep/src/test_elp_transitive_dep_private.hrl").
-include("test_elp_test_utils.hrl").

-export([
    suite/0,
    all/0]).


-export([test_callback_update/1]).


suite() -> [{timetrap, {seconds, 600}}].

all() -> [test_callback_update].


test_callback_update(_Config) -> true.

foo() -> test_elp_test_utils:util().


