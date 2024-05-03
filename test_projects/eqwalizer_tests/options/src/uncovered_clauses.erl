%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(uncovered_clauses).

-compile([export_all, nowarn_export_all]).

-spec uncovered_1(ok) -> ok.
uncovered_1(A) when A == a -> A;
uncovered_1(A) -> A.

-spec uncovered_2(term(), {ok, atom()}) -> ok.
uncovered_2(_, {a, A}) -> A;
uncovered_2(_, _) -> ok.

-spec uncovered_3({ok, atom()}) -> ok.
uncovered_3({ok, _A}) -> ok;
uncovered_3({ok, A}) -> A;
uncovered_3(_) -> ok.

-spec uncovered_4_no_err(ok) -> ok.
uncovered_4_no_err(V) -> V;
uncovered_4_no_err(_) -> ok.

-spec uncovered_5({ok, atom()}) -> ok.
uncovered_5({err, _A}) -> ok;
uncovered_5(_) -> ok.

-spec uncovered_6(#{atom() => atom()}) -> ok.
uncovered_6(#{a := V}) when is_integer(V) -> V;
uncovered_6(_) -> ok.
