-module(app_a).
% elp:ignore no_nowarn_suppressions
-compile([export_all, nowarn_export_all]).
-typing([eqwalizer]).

-define(OK, error).

-spec test1() -> ok.
test1() ->
    ?OK.

-spec test2_neg() -> ok.
test2_neg() ->
    error.

-spec test3_neg() -> ok.
test3_neg() ->
    _ = 3 * an_atom, ok.

-spec test3() -> ok.
test3() ->
    case rand:uniform(2) of
        1 ->
            unspecced();
        2 ->
            test2_neg();
        3 ->
            app_a_mod2:id(ok);
        4 ->
            app_a_mod2:unspecced(ok)
    end.

-spec test4([a]) -> app_a_mod2:alias(a).
test4(L) ->
    [H | _] = lists:map(
      fun app_a_mod2:id/1,
      L),
    _  = atom_to_list(H),
    {H}.

-spec test5() -> ok.
test5() ->
    % so we pull in a lot of deps
    _ = gen_server:call(?MODULE, req),
    meinong:meinong(2).

unspecced() -> anything.

-spec test2_neg_ignored() -> ok.
test2_neg_ignored() ->
    % eqwalizer:fixme it wasn't me
    error.

-spec test6() -> ok.
test6() ->
    % eqwalizer:fixme redundant issue should be reported
    ok.

-spec misc_mismatch_1_neg(
   #{
       k_ok => any(),
       k_wrong1 => pid(),
       k_wrong2 => pid(),
       k_req1 => any(),
       k_req2 => any(),
       k_extra => any()
   }
) ->
    #{
        k_ok => any(),
        k_wrong1 => atom(),
        k_wrong2 => atom(),
        k_req1 := atom(),
        k_req2 := atom(),
        k_req3 := atom()
    }.
misc_mismatch_1_neg(X) ->
    X.

-type id(T) :: T.

-spec misc_mismatch_2_neg(
    id(#{
        a := va,
        b := #{
            c := #{
                d => atom()
            }
        }
    })
) ->
    #{
        a := va,
        b := #{
            c := id(#{
                d := atom(),
                e := atom()
            })
        }
    }.
misc_mismatch_2_neg(X) ->
    X.

-spec misc_nested_1_neg(
    id(#{
        a := va,
        b := #{
            c := #{
                d := pid(),
                e := pid()
            }
        }
    })
) ->
    #{
        a := va,
        b := #{
            c := id(#{
                d := atom(),
                e := atom()
            })
        }
    }.
misc_nested_1_neg(X) ->
    X.
