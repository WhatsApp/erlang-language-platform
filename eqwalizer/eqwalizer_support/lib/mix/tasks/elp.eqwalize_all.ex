defmodule Mix.Tasks.Elp.EqwalizeAll do
  @shortdoc "Type-checks the project with eqWAlizer, through ELP"

  @moduledoc """
  Compiles the project and runs `elp eqwalize-all` in it.

      mix elp.eqwalize_all [ELP_ARGS...]

  Any arguments are passed on to `elp eqwalize-all`, such as `--format json`
  or `--bail-on-error`, which makes the run fail on a type error. ELP is `elp`
  on `PATH`, or the executable the `ELP` environment variable names; it must
  be a build that serves Elixir modules. The task exits with ELP's status.

  The project must have been described to ELP with `mix elp.build_info`. The
  task leaves that description alone, since it may be under version control:
  rerun `mix elp.build_info` after adding modules or dependencies.
  """

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    EqwalizerSupport.Elp.run(["eqwalize-all" | args])
  end
end
