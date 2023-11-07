%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
-module(elp_metadata).
-export([insert_metadata/2]).

insert_metadata(Metadata, Forms) ->
    Attr = {attribute, {1, 1}, elp_metadata, Metadata},
    insert_attr_after_module_attr(Attr, Forms).

% invariants tested by erlang_service_verify
insert_attr_after_module_attr(Attr, Forms) ->
    Reversed = lists:foldl(
        fun (ModuleAttr = {attribute, _, module, _}, Acc) ->
            [Attr, ModuleAttr] ++ Acc;
        (Form, Acc) ->
            [Form | Acc]
        end,
        [],
        Forms
    ),
    lists:reverse(Reversed).
