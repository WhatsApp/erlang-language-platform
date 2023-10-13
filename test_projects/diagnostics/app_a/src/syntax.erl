-module(syntax).

-export([blah/2]).

-spec blah(Pred :: fun ((T) -> erlang:boolean), List :: [T]) -> erlang:boolean().
blah(0,_Y) -> 1;
blah(X,_Y) -> X + 1.

foo() ->
    bug( +
    blah.
