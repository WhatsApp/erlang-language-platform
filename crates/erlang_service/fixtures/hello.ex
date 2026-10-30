defmodule Hello do
  @moduledoc "A trivial module to serve to ELP."
  defstruct [:name, age: 0]

  @type t :: %__MODULE__{name: String.t() | nil, age: non_neg_integer()}

  @spec add(integer(), integer()) :: integer()
  def add(a, b), do: a + b

  @spec greet(String.t()) :: String.t()
  def greet(name), do: "Hello, " <> name

  def bad(), do: add("x", 1)

  def uni(a), do: {"zażółć", a}

  def concat(a, b), do: a ++ b

  def subtract(a, b), do: a -- b

  defmodule Nested do
    def n, do: 1
  end
end
