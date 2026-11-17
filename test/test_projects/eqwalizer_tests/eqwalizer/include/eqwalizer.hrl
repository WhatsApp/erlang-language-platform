%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-ifndef(__EQWALIZER_HRL__).
-define(__EQWALIZER_HRL__, 1).

-ifdef(ELP_ERLANG_SERVICE).
-define(checked_cast(Expr, Type), (Expr :: Type)).
-define(unchecked_cast(Expr, Type), (Expr :> Type)).
-define(dyn_pattern, :?).
-else.
-define(checked_cast(Expr, _Type), Expr).
-define(unchecked_cast(Expr, _Type), Expr).
-define(dyn_pattern, _).
-endif.

-endif.
