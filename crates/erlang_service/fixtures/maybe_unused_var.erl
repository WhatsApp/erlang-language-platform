-module(maybe_unused_var).
-export([unused_case/2, unused_else/2, unused_receive/1, used/2, used_else/2]).

unused_case(Unused, X) ->
    case X of
        a -> maybe ok end;
        _ -> b
    end.

unused_else(Unused, X) ->
    case X of
        a -> maybe ok ?= X else _ -> error end;
        _ -> b
    end.

unused_receive(X) ->
    Unused = X(),
    receive
        a -> maybe ok end
    end.

used(Used, X) ->
    case X of
        a -> maybe Used end;
        _ -> b
    end.

used_else(Used, X) ->
    case X of
        a -> maybe ok ?= X else _ -> Used end;
        _ -> b
    end.
