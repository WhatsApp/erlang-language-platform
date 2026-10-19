defmodule EqwalizerSupport.MixProject do
  use Mix.Project

  # Kept in step with src/eqwalizer_support.app.src, which rebar3 builds read.
  @version "0.9.0"
  @source_url "https://github.com/WhatsApp/erlang-language-platform"

  def project do
    [
      app: :eqwalizer_support,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: false,
      deps: [],
      description:
        "API and spec overrides for the eqWAlizer type checker, " <>
          "and the mix tasks that describe a project to ELP",
      package: package()
    ]
  end

  def application do
    [extra_applications: []]
  end

  defp package do
    [
      files: ~w(lib src mix.exs rebar.config rebar.lock README.md AGENTS.md),
      licenses: ["MIT", "Apache-2.0"],
      links: %{"GitHub" => @source_url}
    ]
  end
end
