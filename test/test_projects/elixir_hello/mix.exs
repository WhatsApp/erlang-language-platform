defmodule ElixirHello.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir_hello,
      version: "0.1.0",
      elixir: "~> 1.15",
      deps: [
        {:eqwalizer_support,
         path: "../../../eqwalizer/eqwalizer_support", only: :dev, runtime: false}
      ]
    ]
  end

  def application do
    [extra_applications: []]
  end
end
