defmodule ElixirService.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir_service,
      version: "0.1.0",
      elixir: "~> 1.18",
      elixirc_paths: ["lib"],
      start_permanent: false,
      deps: deps(),
      escript: escript()
    ]
  end

  def application do
    [extra_applications: []]
  end

  defp deps do
    [{:erlang_service, path: "../erlang_service", manager: :rebar3, runtime: false}]
  end

  defp escript do
    [
      main_module: ElixirService,
      name: "elixir_service",
      path: "_build/elixir_service",
      # Nothing starts before `ElixirService.main/1`: it registers the Elixir
      # handlers and only then starts `erlang_service`, which opens stdio.
      app: nil,
      emu_args: "+sbtu +A0 +sbwt none +sbwtdcpu none +sbwtdio none -noinput -mode minimal +Q65536"
    ]
  end
end
