defmodule Hello do
  @moduledoc "One planted type error, and one that the overrides prevent."

  @spec add(integer(), integer()) :: integer()
  def add(a, b), do: a + b

  @spec bad() :: integer()
  def bad(), do: add("x", 1)

  # Elixir's own spec of `Enum.reduce/3` returns `acc()`, that is `any()`,
  # which does not fit `integer()`; the override gives it a type variable.
  @spec total([integer()]) :: integer()
  def total(xs), do: Enum.reduce(xs, 0, fn x, acc -> x + acc end)

  # Elixir compiles `++` to a call of `:erlang.++/2`, whose spec would give
  # `[term()]`; served as the operator, it keeps the element types.
  @spec concat([integer()], [atom()]) :: [integer() | atom()]
  def concat(a, b), do: a ++ b

  defmodule Nested do
    @spec n() :: integer()
    def n(), do: 1
  end
end
