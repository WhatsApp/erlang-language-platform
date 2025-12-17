-module(broken_parse_trans).

-export([main/0]).

-record(a, {}).

-include_lib("stdlib/include/ms_transform.hrl").

main() ->
  ets:fun2ms(fun(#a{b = B}) -> B end).
