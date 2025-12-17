-module(app_b).
-export([
    test1_neg/1,
    test2/2,
    test3/1

]).
-export_type([
      tup/1


]).

-spec test1_neg([T]) -> T.
test1_neg(L) ->
    L.

-spec test1_neg_ignored([T]) -> T.
test1_neg_ignored(L) ->
  % eqwalizer:fixme
  L.

-spec test2(number(), number()) -> number().
test2(N1, N2) ->
    N1 + N2.

-type tup(T) :: {T}.

-spec test3(any()) -> map().
test3(M) ->
    try
        % eqwalizer:ignore - we do want this behaviour
        maps:update(caller, ?MODULE, M)
    catch
        _ -> #{caller => ?MODULE}
    end.
