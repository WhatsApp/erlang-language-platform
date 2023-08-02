-module(test_elp_test_utils).
-oncall("whatsapp_dev_infra").

-include("test_elp.hrl").
-include_lib("test_elp_transitive_dep/include/test_elp_transitive_dep.hrl").
-include_lib("test_elp_direct_dep/include/test_elp_direct_dep.hrl").
-include_lib("test_elp_no_private_headers/include/test_elp_no_private_headers.hrl").

-export([
    util/0]).

util() -> true.