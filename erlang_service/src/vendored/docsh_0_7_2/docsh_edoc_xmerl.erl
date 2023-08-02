-module(docsh_edoc_xmerl).

%% xmerl:simple_export/2 API
-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

%% EDoc formatter
-export([format_edoc/2]).

-export_type([xml_element_content/0]).

-include_lib("xmerl/include/xmerl.hrl").

%% @type xml_element_content(). `#xmlElement.content' as defined by `xmerl.hrl'.
-type xml_element_content() :: [#xmlElement{} | #xmlText{} | #xmlPI{} | #xmlComment{} | #xmlDecl{}].

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%%
%%' xmerl:simple_export/2 API
%%

-spec '#xml-inheritance#'() -> list().
'#xml-inheritance#'() -> [].

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), any(), any()) -> docsh_internal:t().
'#root#'([#xmlElement{name = module} = Module], _, _, _) ->
    #{name => get_module_name(Module),
      description => get_module_description(Module),
      items => get_functions(Module) ++ get_types(Module)}.

-spec '#element#'(any(), any(), any(), any(), any()) -> any().
'#element#'(_, _, _, _, E) -> E.

%% The '#text#' function is called for every text segment.
-spec '#text#'(any()) -> any().
'#text#'(Text) -> ?il2b(Text).

%%.
%%' xmerl:simple_export/2 helpers
%%

get_module_name(#xmlElement{attributes = Attrs}) ->
    ?l2ea('find_attribute!'(name, Attrs)).

get_module_description(#xmlElement{name = module} = M) ->
    get_description(M).

-spec get_functions(#xmlElement{}) -> [docsh_internal:item()].
get_functions(#xmlElement{name = module} = M) ->
    get_content(functions, [], fun get_functions/1, M);
get_functions(#xmlElement{name = functions, content = Content}) ->
    [ get_function(Function) || #xmlElement{name = function} = Function <- Content ].

-spec get_function(#xmlElement{}) -> docsh_internal:item().
get_function(#xmlElement{attributes = Attrs} = Function) ->
    #{kind        => 'function',
      name        => ?l2ea('find_attribute!'(name, Attrs)),
      arity       => ?l2i('find_attribute!'(arity, Attrs)),
      exported    => list_to_boolean('find_attribute!'(exported, Attrs)),
      description => get_function_description(Function)}.

-spec get_types(#xmlElement{}) -> [docsh_internal:item()].
get_types(#xmlElement{name = module} = M) ->
    get_content(typedecls, [], fun get_types/1, M);
get_types(#xmlElement{name = typedecls, content = Content}) ->
    [ get_type(Type) || #xmlElement{name = typedecl} = Type <- Content ].

-spec get_type(#xmlElement{}) -> docsh_internal:item().
get_type(#xmlElement{name = typedecl} = Type) ->
    #{kind        => 'type',
      name        => get_type_name(Type),
      arity       => get_type_arity(Type),
      %% TODO: really always true? anyway, we want the structure for functions and types
      %% to be the same
      exported    => true,
      description => get_type_description(Type)}.

get_function_description(#xmlElement{name = function} = Function) ->
    get_description(Function).

get_type_name(#xmlElement{name = typedecl} = Type) ->
    get_type_def(fun get_type_name/1, Type);
get_type_name(#xmlElement{name = typedef} = TypeDef) ->
    case get_content(erlangName, {error, no_erlang_name}, fun get_type_name/1, TypeDef) of
        {error, no_erlang_name} -> erlang:error({not_found, erlangName});
        TypeName -> TypeName
    end;
get_type_name(#xmlElement{name = erlangName, attributes = Attrs}) ->
    ?l2ea('find_attribute!'(name, Attrs)).

get_type_arity(#xmlElement{name = typedecl} = Type) ->
    get_type_def(fun get_type_arity/1, Type);
get_type_arity(#xmlElement{name = typedef} = TypeDef) ->
    case get_content(argtypes, {error, no_argtypes}, fun get_type_arity/1, TypeDef) of
        {error, no_argtypes} -> erlang:error({not_found, argtypes});
        TypeArity -> TypeArity
    end;
get_type_arity(#xmlElement{name = argtypes, content = Content}) ->
    count_args(Content).

count_args(Args) ->
    length([ Arg || #xmlElement{name = type} = Arg <- Args ]).

get_type_description(#xmlElement{name = typedecl} = Type) ->
    get_description(Type).

get_content(Name, Default, ContinueFun, #xmlElement{content = Content} = Element) ->
    case lists:keyfind(Name, #xmlElement.name, Content) of
        false -> debug({not_found, Name}, Element),
                 Default;
        #xmlElement{} = Found -> ContinueFun(Found)
    end.

get_description(#xmlElement{} = Element) ->
    get_content(description, none, fun get_full_description/1, Element).

get_full_description(#xmlElement{name = description} = D) ->
    get_content(fullDescription, none, fun get_full_description/1, D);
get_full_description(#xmlElement{name = fullDescription, content = XmlElementContent}) ->
    %% See xmerl.hrl for the definition of #xmlElement.content:
    %%   content = [#xmlElement()|#xmlText()|#xmlPI()|#xmlComment()|#xmlDecl()]
    format_text(XmlElementContent).

format_text(TextSubtree) ->
    %% Just return the EDoc subtree for storage or later processing.
    TextSubtree.

get_type_def(ContinueFun, #xmlElement{name = typedecl} = Type) ->
    case get_content(typedef, {error, no_typedef}, ContinueFun, Type) of
        {error, no_typedef} -> erlang:error({not_found, typedef, Type});
        ContinuationResult -> ContinuationResult
    end.

list_to_boolean("yes") -> true;
list_to_boolean("no")  -> false.

'find_attribute!'(Attr, Attrs) ->
    case xmerl_lib:find_attribute(Attr, Attrs) of
        false -> erlang:error({no_attribute, Attr, Attrs});
        {value, Value} -> Value
    end.

%% Intended only for tracing.
debug(_, _) -> ok.

%%.
%%' EDoc formatter
%%

-spec format_edoc(xml_element_content(), any()) -> iolist().
format_edoc(Content, Ctx) ->
    lists:map(fun
                  ({br})        -> "\n";
                  ({i, Inline}) -> [Inline]
              end, end_block(format_content(Content, Ctx))).

format_content(Content, Ctx) ->
    lists:flatten([ format_content_(C, Ctx) || C <- Content ]).

format_content_(#xmlPI{}, _Ctx)      -> [];
format_content_(#xmlComment{}, _Ctx) -> [];
format_content_(#xmlDecl{}, _Ctx)    -> [];

format_content_(#xmlText{} = T, Ctx) ->
    Text = T#xmlText.value,
    case edoc_lib:is_space(Text) of
        true -> [];
        false ->
            case is_preformatted(T#xmlText.parents) of
                true  -> cleanup_preformatted_text(Text, Ctx);
                false -> cleanup_text(Text, Ctx)
            end
    end;

format_content_(#xmlElement{name = Name, content = Content} = E, Ctx) ->
    format_element(Name, E, format_content(Content, Ctx), Ctx).

format_element(h1, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h2, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h3, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h4, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h5, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h6, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(hgroup, _, _Lines, _Ctx) -> [];
format_element(code, #xmlElement{}, Lines, _Ctx) ->
    Lines;
format_element(dl, #xmlElement{}, Lines, _Ctx) ->
    end_block(Lines);
format_element(dt, #xmlElement{}, Lines, _Ctx) ->
    dl_item("  ", Lines);
format_element(dd, #xmlElement{}, Lines, _Ctx) ->
    dl_item("      ", Lines);
format_element(p, #xmlElement{}, Lines, _Ctx) ->
    end_block(lists:dropwhile(fun
                                  ({br}) -> true;
                                  (_) -> false
                              end, Lines));
format_element(pre, #xmlElement{}, Lines, _Ctx) ->
    end_block(Lines);
format_element(ol, #xmlElement{} = E, ListItems, Ctx) ->
    lists:all(fun ({li, _}) -> true; (_) -> false end, ListItems)
        orelse erlang:error({non_list_item_children, ListItems}, [ol, E, ListItems, Ctx]),
    end_block([ [{i, io_lib:format("  ~b. ", [Index])}, FirstItem, IndentedRest, {br}]
                || {Index, {li, [FirstItem | Rest]}} <- enumerate(ListItems),
                   IndentedRest <- [prepend("    ", Rest)] ]);
format_element(ul, #xmlElement{} = E, ListItems, Ctx) ->
    lists:all(fun ({li, _}) -> true; (_) -> false end, ListItems)
        orelse erlang:error({non_list_item_children, ListItems}, [ul, E, ListItems, Ctx]),
    end_block([ [{i, "  - "}, FirstItem, IndentedRest, {br}]
                || {li, [FirstItem | Rest]} <- ListItems,
                   IndentedRest <- [prepend("    ", Rest)] ]);
format_element(li, #xmlElement{}, Lines, _Ctx) ->
    [{li, Lines}];
format_element(_, #xmlElement{}, Lines, _Ctx) ->
    Lines.

format_header(#xmlElement{name = Name}, Lines, _Ctx) ->
    Headers = #{h1 => "# ",
                h2 => "## ",
                h3 => "### ",
                h4 => "#### ",
                h5 => "##### ",
                h6 => "###### "},
    case Name of
        hgroup -> [];
        _ ->
            [{i, Text}] = Lines,
            end_block([{br}, {i, [maps:get(Name, Headers), Text]}])
    end.

cleanup_text(Text, _Ctx) ->
    lists:flatmap(fun
                      ("\n") -> [{br}];
                      (T) ->
                          case edoc_lib:is_space(T) of
                              true -> [];
                              false -> [{i, T}]
                          end
                  end,
                  split(Text, "\s*(\n)\s*", [trim, {return, list}])).

cleanup_preformatted_text(Text, _Ctx) ->
    lists:flatmap(fun
                      ("\n") -> [{br}];
                      (T) -> [{i, T}]
                  end,
                  split(Text, "(\n)", [{return, list}])).

split(Text, Pattern, Opts) ->
    re:split(Text, Pattern, Opts).

is_preformatted(Parents) ->
    lists:any(fun
                  ({pre, _}) -> true;
                  (_) -> false
              end, Parents).

prepend(Prefix, Doc) -> prepend(Prefix, lists:reverse(Doc), []).

prepend(_Prefix, [], Acc) -> Acc;
prepend( Prefix, [{br} | Doc], [] = Acc) -> prepend(Prefix, Doc, [{br} | Acc]);
prepend( Prefix, [{br} | Doc], [{br}] = Acc) -> prepend(Prefix, Doc, [{br} | Acc]);
prepend( Prefix, [{br} | Doc], [{br}, {br}] = Acc) -> prepend(Prefix, Doc, Acc);
prepend( Prefix, [{br} | Doc], Acc) -> prepend(Prefix, Doc, [{br}, {i, Prefix} | Acc]);
prepend( Prefix, [Node | Doc], Acc) -> prepend(Prefix, Doc, [Node | Acc]).

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

end_block(Doc) -> end_block([{br}, {br} | lists:reverse(lists:flatten(Doc))], []).

end_block([], Acc) -> Acc;
end_block([{br} | Doc], [] = Acc) -> end_block(Doc, [{br} | Acc]);
end_block([{br} | Doc], [{br}] = Acc) -> end_block(Doc, [{br} | Acc]);
end_block([{br} | Doc], [{br}, {br}] = Acc) -> end_block(Doc, Acc);
end_block([Node | Doc], Acc) -> end_block(Doc, [Node | Acc]).

dl_item(Prefix, Lines) ->
    [First | Rest] = Lines,
    end_block([{i, Prefix}, First, prepend(Prefix, Rest)]).

%%. vim: foldmethod=marker foldmarker=%%',%%.