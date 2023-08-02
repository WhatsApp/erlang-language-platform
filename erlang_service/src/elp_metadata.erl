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
