%% Spec overrides for the Elixir standard library, for better type-checking
%% with eqWAlizer.
%%
%% Elixir specs give functions that return their accumulator or container the
%% result type `acc()` or `map()`, so a struct threaded through `Enum.reduce`
%% or `update_in` comes out as `term()` and fits nowhere. These specs use type
%% variables where ELP's bundled specs for `lists` and `maps` do, so the input
%% type is the output type, and `dynamic()` for the elements and values Elixir
%% declares as `any()`. Only functions whose Elixir spec causes such noise are
%% overridden; a function without a spec is dynamically typed already.
%%
%% Every function is named 'Module:function', like ELP's bundled
%% eqwalizer_specs, which these are layered over. `mix elp.build_info` copies
%% this module into the project's build directory and lists it as a dependency.
-module(eqwalizer_specs).
-compile(warn_missing_spec).
-compile([export_all, nowarn_export_all]).

%% -------- Elixir.Access --------

-spec 'Elixir.Access:fetch'(term(), term()) -> {ok, dynamic()} | error.
'Elixir.Access:fetch'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Access:get'(term(), term(), Default) -> dynamic() | Default.
'Elixir.Access:get'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Access:get_and_update'(Data, term(), fun((dynamic()) -> {Get, dynamic()} | pop)) ->
    {Get, Data}.
'Elixir.Access:get_and_update'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Access:pop'(Data, term()) -> {dynamic(), Data}.
'Elixir.Access:pop'(_, _) -> error(eqwalizer_specs).

%% -------- Elixir.Enum --------

-spec 'Elixir.Enum:concat'(term()) -> [dynamic()].
'Elixir.Enum:concat'(_) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:concat'(term(), term()) -> [dynamic()].
'Elixir.Enum:concat'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:each'(term(), fun((dynamic()) -> term())) -> ok.
'Elixir.Enum:each'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:flat_map_reduce'(term(), Acc, fun((dynamic(), Acc) -> {term(), Acc} | {halt, Acc})) ->
    {[dynamic()], Acc}.
'Elixir.Enum:flat_map_reduce'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:into'(term(), Collectable) -> Collectable.
'Elixir.Enum:into'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:into'(term(), Collectable, fun((dynamic()) -> dynamic())) -> Collectable.
'Elixir.Enum:into'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:map'(term(), fun((dynamic()) -> T)) -> [T].
'Elixir.Enum:map'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:map_reduce'(term(), Acc, fun((dynamic(), Acc) -> {dynamic(), Acc})) ->
    {[dynamic()], Acc}.
'Elixir.Enum:map_reduce'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:reduce'(term(), Acc, fun((dynamic(), Acc) -> Acc)) -> Acc.
'Elixir.Enum:reduce'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:reduce_while'(term(), Acc, fun((dynamic(), Acc) -> {cont, Acc} | {halt, Acc})) -> Acc.
'Elixir.Enum:reduce_while'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Enum:zip'(term(), term()) -> [{dynamic(), dynamic()}].
'Elixir.Enum:zip'(_, _) -> error(eqwalizer_specs).

%% -------- Elixir.GenServer --------

-spec 'Elixir.GenServer:call'(term(), term(), timeout()) -> dynamic().
'Elixir.GenServer:call'(_, _, _) -> error(eqwalizer_specs).

%% -------- Elixir.Kernel --------

-spec 'Elixir.Kernel:apply'(fun(), [term()]) -> dynamic().
'Elixir.Kernel:apply'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:apply'(module(), atom(), [term()]) -> dynamic().
'Elixir.Kernel:apply'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:elem'(tuple(), non_neg_integer()) -> dynamic().
'Elixir.Kernel:elem'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:get_and_update_in'(Data, [term(), ...], fun((dynamic()) -> {Get, dynamic()} | pop)) ->
    {Get, Data}.
'Elixir.Kernel:get_and_update_in'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:get_in'(term(), [term(), ...]) -> dynamic().
'Elixir.Kernel:get_in'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:pop_in'(Data, [term(), ...]) -> {dynamic(), Data}.
'Elixir.Kernel:pop_in'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:put_in'(Data, [term(), ...], term()) -> Data.
'Elixir.Kernel:put_in'(_, _, _) -> error(eqwalizer_specs).

%% A struct of the module given, which eqWAlizer cannot express.
-spec 'Elixir.Kernel:struct'(module() | map(), term()) -> dynamic().
'Elixir.Kernel:struct'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:struct!'(module() | map(), term()) -> dynamic().
'Elixir.Kernel:struct!'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Kernel:update_in'(Data, [term(), ...], fun((dynamic()) -> dynamic())) -> Data.
'Elixir.Kernel:update_in'(_, _, _) -> error(eqwalizer_specs).

%% -------- Elixir.List --------

-spec 'Elixir.List:wrap'(term()) -> [dynamic()].
'Elixir.List:wrap'(_) -> error(eqwalizer_specs).

%% -------- Elixir.Map --------

%% `update_in/2` and `put_in/2` on a struct field compile to these.

-spec 'Elixir.Map:get_and_update!'(Map, term(), fun((dynamic()) -> {Get, dynamic()} | pop)) -> {Get, Map}.
'Elixir.Map:get_and_update!'(_, _, _) -> error(eqwalizer_specs).

-spec 'Elixir.Map:update!'(Map, term(), fun((dynamic()) -> dynamic())) -> Map.
'Elixir.Map:update!'(_, _, _) -> error(eqwalizer_specs).

%% -------- Elixir.Process --------

-spec 'Elixir.Process:get'(term(), term()) -> dynamic().
'Elixir.Process:get'(_, _) -> error(eqwalizer_specs).

-spec 'Elixir.Process:put'(term(), term()) -> dynamic() | nil.
'Elixir.Process:put'(_, _) -> error(eqwalizer_specs).

%% -------- Elixir.Task --------

-spec 'Elixir.Task:await'(term(), timeout()) -> dynamic().
'Elixir.Task:await'(_, _) -> error(eqwalizer_specs).
