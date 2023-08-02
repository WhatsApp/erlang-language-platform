%% @doc EEP-0048 (http://erlang.org/eeps/eep-0048.html) docs_v1 documentation format support.
-module(docsh_docs_v1).

-behaviour(docsh_format).
-export([lookup/3]).

-behaviour(docsh_writer).
-export([from_internal/1]).

-export_type([t/0,
              item/0]).

-record(docs_v1, {anno,
                  beam_language,
                  format,
                  module_doc,
                  metadata,
                  docs}).

-opaque t() :: #docs_v1{anno :: erl_anno:anno(),
                        beam_language :: atom(),
                        format :: mime_type(),
                        module_doc :: i18n_doc() | none | hidden,
                        metadata :: map(),
                        docs :: [item()]}.

-type mime_type() :: binary().

-type item() :: {KNA :: docsh_format:kna(),
                 Anno :: erl_anno:anno(),
                 Signature :: [binary()],
                 Doc :: i18n_doc() | none | hidden,
                 Metadata :: map()}.

-type i18n_doc() :: #{language() := string_()}.
-type language() :: binary().
-type string_() :: binary().

-define(il2b(IOList), iolist_to_binary(IOList)).

-spec lookup(Docs, Key, Kinds) -> R when
      Docs  :: docsh_format:t(),
      Key   :: docsh_format:key(),
      Kinds :: docsh_format:kinds(),
      R :: {ok, [item()]}
         | {not_found, docsh_format:error_message()}.
lookup(#docs_v1{} = Docs, Key, Kinds0) ->
    %docsh_lib:print("lookup ~p ~p in\n  ~p\n", [Key, Kinds0, Docs]),
    %% This way we'll never get [spec, doc], only [doc, spec].
    Kinds = lists:sort(Kinds0),
    dispatch_lookup(Docs, Key, Kinds).

dispatch_lookup(Docs, _Mod, [moduledoc]) ->
    case Docs#docs_v1.module_doc of
        none   -> {not_found, docsh_format:module_doc_not_available()};
        hidden -> {not_found, docsh_format:module_doc_hidden()};
        Doc    -> {ok, [Doc]}
    end;
dispatch_lookup(Docs, {_Mod, Name, Arity}, [doc, type]) ->
    case fetch_items(Docs#docs_v1.docs, type, Name, Arity) of
        []    -> {not_found, docsh_format:item_doc_not_available()};
        Items -> {ok, Items}
    end;
dispatch_lookup(Docs, _Mod, [type]) ->
    case fetch_items(Docs#docs_v1.docs, type, any, any) of
        []    -> {not_found, docsh_format:item_doc_not_available()};
        Items -> {ok, Items}
    end;
dispatch_lookup(Docs, {_Mod, Name, Arity}, Kinds)
  when Kinds =:= [doc, spec];
       Kinds =:= [spec] ->
    case fetch_items(Docs#docs_v1.docs, function, Name, Arity) of
        []    -> {not_found, docsh_format:item_doc_not_available()};
        Items -> {ok, Items}
    end.

fetch_items(AllItems, Kind, Name, Arity) ->
    lists:filter(mk_select(Kind, Name, Arity), AllItems).

mk_select(Kind, Name, Arity) ->
    fun ({ItemKNA, _, _, _, _}) -> select(Kind, Name, Arity, ItemKNA) end.

select(Kind, Name, Arity, {Kind, ItemName, ItemArity})
  when Name =:= ItemName orelse Name =:= any,
       Arity =:= ItemArity orelse Arity =:= any ->
    true;
select(_, _, _, _) ->
    false.

-spec from_internal(docsh_internal:t()) -> t().
from_internal(Internal) ->
    %% TODO: remove
    %docsh_lib:print("internal ~p\n", [Internal]),
    ModuleName = maps:get(name, Internal),
    Description = maps:get(description, Internal, none),
    %% TODO: this contains some assumptions, e.g. English language
    Lang = <<"en">>,
    ModuleInfo = #{name => ModuleName,
                   lang => Lang},
    Docs0 =
        (docs_v1_default())#docs_v1{format = <<"text/erlang-edoc">>,
                                    module_doc = module_doc(Lang, Description)},
    %% TODO: it would be nice to get source location for the annotation here
    {Docs, DocsMap} = lists:foldl(mk_step(ModuleInfo),
                                  {Docs0, #{}},
                                  maps:get(items, Internal)),
    %docsh_lib:print("Docs : ~p\n", [Docs]),
    %docsh_lib:print("DocsMap: ~p\n", [DocsMap]),
    Docs#docs_v1{docs = maps:values(DocsMap)}.

docs_v1_default() ->
    #docs_v1{anno = erl_anno:new({0, 1}),
             beam_language = erlang,
             format = <<"text/plain">>,
             module_doc = none,
             metadata = #{},
             docs = []}.

module_doc(_Lang, none) -> none;
module_doc( Lang, D) -> #{Lang => D}.

mk_step(ModuleInfo) ->
    fun (Info, Acc) -> step(ModuleInfo, Info, Acc) end.

step(ModuleInfo, Info, { #docs_v1{} = DocsV1, DocsMap }) ->
    %docsh_lib:print("item: ~p\n", [Info]),
    %ct:pal("item: ~p\n", [Info]),
    {_, Name, Arity} = KNA = docsh_internal:kna(Info),
    Entry = { KNA,
              erl_anno:new({0, 1}),
              signature(ModuleInfo, Info),
              description(Name, Arity, Info, ModuleInfo),
              #{} },
    {DocsV1, DocsMap#{KNA => Entry}}.

signature(ModuleInfo, Info) ->
    case maps:get(signature, Info, no_signature) of
        no_signature ->
            %% TODO: this is kind of lame
            %% as with the current format function it displays the same text twice
            kna_signature(ModuleInfo, Info);
        Signature ->
            Signature
    end.

kna_signature(ModuleInfo, Info) ->
    #{name := Mod} = ModuleInfo,
    {_, Name, Arity} = docsh_internal:kna(Info),
    ?il2b(io_lib:format("~s:~s/~p\n", [Mod, Name, Arity])).

description(_Name, _Arity, Info, #{lang := Lang}) ->
    case maps:get(description, Info, none) of
        none -> none;
        D -> #{Lang => D}
    end.