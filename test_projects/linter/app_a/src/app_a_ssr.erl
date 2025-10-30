-module(app_a_ssr).

-export([bar/1, bar2/0]).

%% Used for testing visible parens matching
bar(X) ->
     X = ((3)),
     X = (4).

-define(BAR(X), {X}).
bar2() -> ?BAR(4).
