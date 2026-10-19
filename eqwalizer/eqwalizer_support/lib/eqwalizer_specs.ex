defmodule :eqwalizer_specs do
  @moduledoc """
  Spec overrides for the Elixir standard library, for better type-checking
  with eqWAlizer.

  The module is named as ELP expects its override module to be, and every
  function is named `"Module:function"`, like the overrides for OTP in
  `src/eqwalizer_specs.erl`, which Erlang projects use through rebar3 and
  which mix leaves alone. Mix compiles this module for an Elixir project, ELP
  reads it from its BEAM file as it reads the project's own modules, and
  layers these entries over its bundled copy of the OTP overrides, entry by
  entry, so the two never meet, and Dialyzer never sees this one.

  Elixir specs give functions that return their accumulator or container the
  result type `acc()` or `map()`, so a struct threaded through `Enum.reduce/3`
  or `update_in/2` comes out as `term()` and fits nowhere. These specs use
  type variables where ELP's bundled specs for `:lists` and `:maps` do, so the
  input type is the output type, and `dynamic()` for the elements and values
  Elixir declares as `any()`. Only functions whose Elixir spec causes such
  noise are overridden; a function without a spec is dynamically typed already.
  """

  # Access

  @spec unquote(:"Elixir.Access:fetch")(term, term) :: {:ok, dynamic} | :error
  def unquote(:"Elixir.Access:fetch")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Access:get")(term, term, default) :: dynamic | default
        when default: var
  def unquote(:"Elixir.Access:get")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Access:get_and_update")(data, term, (dynamic -> {get, dynamic} | :pop)) ::
          {get, data}
        when data: var, get: var
  def unquote(:"Elixir.Access:get_and_update")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Access:pop")(data, term) :: {dynamic, data} when data: var
  def unquote(:"Elixir.Access:pop")(_, _), do: :erlang.error(:eqwalizer_specs)

  # Enum

  @spec unquote(:"Elixir.Enum:concat")(term) :: [dynamic]
  def unquote(:"Elixir.Enum:concat")(_), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:concat")(term, term) :: [dynamic]
  def unquote(:"Elixir.Enum:concat")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:each")(term, (dynamic -> term)) :: :ok
  def unquote(:"Elixir.Enum:each")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:flat_map_reduce")(
          term,
          acc,
          (dynamic, acc -> {term, acc} | {:halt, acc})
        ) :: {[dynamic], acc}
        when acc: var
  def unquote(:"Elixir.Enum:flat_map_reduce")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:into")(term, collectable) :: collectable when collectable: var
  def unquote(:"Elixir.Enum:into")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:into")(term, collectable, (dynamic -> dynamic)) :: collectable
        when collectable: var
  def unquote(:"Elixir.Enum:into")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:map")(term, (dynamic -> t)) :: [t] when t: var
  def unquote(:"Elixir.Enum:map")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:map_reduce")(term, acc, (dynamic, acc -> {dynamic, acc})) ::
          {[dynamic], acc}
        when acc: var
  def unquote(:"Elixir.Enum:map_reduce")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  # Elixir declares `Enum.sum/1`, `Enum.sum_by/2`, `Enum.product/1` and
  # `Enum.product_by/2` as returning `number()`, so a sum of integers cannot
  # flow into an `integer()`. With a type variable, integers sum to an integer
  # and floats to a float, and a range sums to an integer. (A bound such as
  # `when n: number` would not do: eqWAlizer substitutes it for the variable.)
  # The empty list sums to the integer 0 and multiplies to 1 whatever its
  # element type, a looseness accepted here. Any other enumerable matches
  # neither clause and gets `dynamic()`.

  @spec unquote(:"Elixir.Enum:product")([n]) :: n when n: var
  @spec unquote(:"Elixir.Enum:product")(Range.t()) :: integer
  def unquote(:"Elixir.Enum:product")(_), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:product_by")(term, (dynamic -> n)) :: n when n: var
  def unquote(:"Elixir.Enum:product_by")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:reduce")(term, acc, (dynamic, acc -> acc)) :: acc when acc: var
  def unquote(:"Elixir.Enum:reduce")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:reduce_while")(
          term,
          acc,
          (dynamic, acc -> {:cont, acc} | {:halt, acc})
        ) :: acc
        when acc: var
  def unquote(:"Elixir.Enum:reduce_while")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:sum")([n]) :: n when n: var
  @spec unquote(:"Elixir.Enum:sum")(Range.t()) :: integer
  def unquote(:"Elixir.Enum:sum")(_), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:sum_by")(term, (dynamic -> n)) :: n when n: var
  def unquote(:"Elixir.Enum:sum_by")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Enum:zip")(term, term) :: [{dynamic, dynamic}]
  def unquote(:"Elixir.Enum:zip")(_, _), do: :erlang.error(:eqwalizer_specs)

  # GenServer

  @spec unquote(:"Elixir.GenServer:call")(term, term, timeout) :: dynamic
  def unquote(:"Elixir.GenServer:call")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  # Kernel

  @spec unquote(:"Elixir.Kernel:apply")(function, [term]) :: dynamic
  def unquote(:"Elixir.Kernel:apply")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:apply")(module, atom, [term]) :: dynamic
  def unquote(:"Elixir.Kernel:apply")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:elem")(tuple, non_neg_integer) :: dynamic
  def unquote(:"Elixir.Kernel:elem")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:get_and_update_in")(
          data,
          [term, ...],
          (dynamic -> {get, dynamic} | :pop)
        ) :: {get, data}
        when data: var, get: var
  def unquote(:"Elixir.Kernel:get_and_update_in")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:get_in")(term, [term, ...]) :: dynamic
  def unquote(:"Elixir.Kernel:get_in")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:pop_in")(data, [term, ...]) :: {dynamic, data} when data: var
  def unquote(:"Elixir.Kernel:pop_in")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:put_in")(data, [term, ...], term) :: data when data: var
  def unquote(:"Elixir.Kernel:put_in")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  # A struct of the module given, which eqWAlizer cannot express.
  @spec unquote(:"Elixir.Kernel:struct")(module | map, term) :: dynamic
  def unquote(:"Elixir.Kernel:struct")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:struct!")(module | map, term) :: dynamic
  def unquote(:"Elixir.Kernel:struct!")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Kernel:update_in")(data, [term, ...], (dynamic -> dynamic)) :: data
        when data: var
  def unquote(:"Elixir.Kernel:update_in")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  # List

  @spec unquote(:"Elixir.List:wrap")(term) :: [dynamic]
  def unquote(:"Elixir.List:wrap")(_), do: :erlang.error(:eqwalizer_specs)

  # Map: `update_in/2` and `put_in/2` on a struct field compile to these.

  @spec unquote(:"Elixir.Map:get_and_update!")(map, term, (dynamic -> {get, dynamic} | :pop)) ::
          {get, map}
        when map: var, get: var
  def unquote(:"Elixir.Map:get_and_update!")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Map:update!")(map, term, (dynamic -> dynamic)) :: map when map: var
  def unquote(:"Elixir.Map:update!")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  # Process

  @spec unquote(:"Elixir.Process:get")(term, term) :: dynamic
  def unquote(:"Elixir.Process:get")(_, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Process:put")(term, term) :: dynamic | nil
  def unquote(:"Elixir.Process:put")(_, _), do: :erlang.error(:eqwalizer_specs)

  # Stream: Elixir bounds the accumulator with `when acc: term`, which
  # eqWAlizer reads as a substitution by `term()` rather than as a type
  # variable.

  @spec unquote(:"Elixir.Stream:transform")(
          term,
          acc,
          (dynamic, acc -> {term, acc} | {:halt, acc})
        ) :: dynamic
        when acc: var
  def unquote(:"Elixir.Stream:transform")(_, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Stream:transform")(
          term,
          (-> acc),
          (dynamic, acc -> {term, acc} | {:halt, acc}),
          (acc -> term)
        ) :: dynamic
        when acc: var
  def unquote(:"Elixir.Stream:transform")(_, _, _, _), do: :erlang.error(:eqwalizer_specs)

  @spec unquote(:"Elixir.Stream:transform")(
          term,
          (-> acc),
          (dynamic, acc -> {term, acc} | {:halt, acc}),
          (acc -> {term, acc} | {:halt, acc}),
          (acc -> term)
        ) :: dynamic
        when acc: var
  def unquote(:"Elixir.Stream:transform")(_, _, _, _, _), do: :erlang.error(:eqwalizer_specs)

  # Task

  @spec unquote(:"Elixir.Task:await")(term, timeout) :: dynamic
  def unquote(:"Elixir.Task:await")(_, _), do: :erlang.error(:eqwalizer_specs)
end
