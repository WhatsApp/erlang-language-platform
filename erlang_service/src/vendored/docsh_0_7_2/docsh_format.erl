-module(docsh_format).

-export([lookup/3,
         module_doc_not_available/0,
         module_doc_hidden/0,
         item_doc_not_available/0,
         item_doc_hidden/0]).

-export_type([t/0,
              key/0,
              kna/0,
              kind/0,
              kinds/0,
              item/0,
              error_message/0]).

-type t() :: term().
-type item() :: term().
-type key() :: module() | kna().
-type kna() :: {kind(), atom(), arity() | 'any'}.
-type kind() :: doc | spec | type | moduledoc.
-type kinds() :: [docsh_internal:kind()].
-type error_message() :: binary().

-callback lookup(Docs, Key, Kinds) -> R when
          Docs  :: docsh_format:t(),
          Key   :: docsh_format:key(),
          Kinds :: docsh_format:kinds(),
          R :: {ok, [docsh_format:item()]}
             | {not_found, docsh_format:error_message()}.

-spec lookup(t(), key(), kinds()) -> {ok, [item()]} | {not_found, error_message()}.
lookup(Docs, Key, Items) ->
    KnownFormats = application:get_env(docsh, extra_docs_chunk_formats, []) ++ default_formats(),
    DocsFormat = element(1, Docs),
    case lists:keyfind(DocsFormat, 1, KnownFormats) of
        {DocsFormat, FormatMod} ->
            FormatMod:lookup(Docs, Key, Items);
        false ->
            erlang:error({unknown_docs_format, DocsFormat}, [Docs, Key, Items])
    end.

default_formats() ->
    [
     {docs_v1, docsh_docs_v1}
    ].

-spec module_doc_not_available() -> error_message().
module_doc_not_available() ->
    <<"Module description is not available.\n">>.

-spec module_doc_hidden() -> error_message().
module_doc_hidden() ->
    <<"Module description is hidden.\n">>.

-spec item_doc_not_available() -> error_message().
item_doc_not_available() ->
    <<"Item description is not available.\n">>.

-spec item_doc_hidden() -> error_message().
item_doc_hidden() ->
    <<"Item description is hidden.\n">>.