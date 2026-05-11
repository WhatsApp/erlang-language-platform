-module(caller).

use_builtin() ->
    apply(target, builtin_target, [42]).

use_custom() ->
    my_rpc:call(node(), target, custom_target, [42]).

use_mock_list() ->
    my_mock:setup([target], #{}).
