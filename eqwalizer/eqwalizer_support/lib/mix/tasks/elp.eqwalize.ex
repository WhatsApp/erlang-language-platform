defmodule Mix.Tasks.Elp.Eqwalize do
  @shortdoc "Type-checks the given modules with eqWAlizer, through ELP"

  @moduledoc """
  Compiles the project and runs `elp eqwalize` on the given modules.

      mix elp.eqwalize MODULE... [ELP_ARGS...]

  Modules are named as in Elixir: `Hello.World`, or `:lists` for an Erlang
  module. ELP checks by file, so a module defined in a file with others is
  checked along with them. Any other arguments are passed on to
  `elp eqwalize`, such as `--format json`. ELP is `elp` on `PATH`, or the
  executable the `ELP` environment variable names; it must be a build that
  serves Elixir modules. The task exits with ELP's status.

  The project must have been described to ELP with `mix elp.build_info`, as
  for `mix elp.eqwalize_all`.
  """

  use Mix.Task

  @impl Mix.Task
  def run([]), do: Mix.raise("mix elp.eqwalize needs at least one module to check")

  def run(args) do
    EqwalizerSupport.Elp.run(["eqwalize" | Enum.map(args, &EqwalizerSupport.Elp.module_name/1)])
  end
end
