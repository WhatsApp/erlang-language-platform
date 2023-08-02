-module(docsh_internal).

-export([merge/1,
         kna/1]).

-export_type([t/0,
              item/0,
              kna/0]).

-type t()    :: #{name        := module(),
                  items       := [item()],
                  description => none | iodata()}.

-type item() :: #{kind        := function | type,
                  name        := atom(),
                  arity       := arity(),
                  description => none | iodata(),
                  exported    => boolean(),
                  signature   => iodata()}.

-type kna() :: {function | type, atom(), arity()}.

-define(a2b(A), atom_to_binary(A, utf8)).
-define(a2l(A), atom_to_list(A)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

%%
%%' Public
%%

-spec merge([Info]) -> MergedInfo when
      Info :: t(),
      MergedInfo :: t().
merge([]) -> [];
merge([Info]) -> Info;
merge([Info1, Info2 | Rest]) ->
    case are_disjoint(Info1, Info2) of
        false -> erlang:error(not_disjoint, [Info1, Info2 | Rest]);
        true ->
            (module(Info1) =:= module(Info2)
             orelse erlang:error(different_modules, [Info1, Info2 | Rest])),
            merge([merge_two(Info1, Info2) | Rest])
    end.

-spec kna(item()) -> kna().
kna(#{kind := K, name := N, arity := A}) -> {K, N, A}.

%%.
%%' Internal
%%

-spec merge_two(t(), t()) -> t().
merge_two(#{items := Items1} = Info1, #{items := Items2}) ->
    ItemsByKNA = dict:to_list( docsh_lib:group_by(fun kna/1, Items1 ++ Items2) ),
    NewItems = lists:map(fun merge_two_/1,  ItemsByKNA),
    Info1#{items := NewItems}.

-spec merge_two_({kna(), [item()]}) -> item().
merge_two_({{Kind, Name, Arity}, [Item]}) ->
    #{kind := Kind, name := Name, arity := Arity} = Item,
    Item;
merge_two_({{Kind, Name, Arity}, [Item1, Item2]}) ->
    #{kind := Kind, name := Name, arity := Arity} = Item1,
    #{kind := Kind, name := Name, arity := Arity} = Item2,
    maps:merge(Item1, Item2).

are_disjoint(Info1, Info2) ->
    Module = maps:get(name, Info1),
    Module = maps:get(name, Info2),
    #{items := Items1} = Info1,
    #{items := Items2} = Info2,
    Items1 -- Items2 == Items1.

module(#{name := Name}) -> Name.

%%. vim: foldmethod=marker foldmarker=%%',%%.