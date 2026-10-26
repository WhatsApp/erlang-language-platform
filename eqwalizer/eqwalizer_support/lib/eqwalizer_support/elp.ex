defmodule EqwalizerSupport.Elp do
  @moduledoc """
  Runs ELP in the project for the `elp.*` mix tasks.
  """

  @description_files ["build_info.json", ".elp.toml"]

  @doc """
  Compiles the project and runs `elp` with `args` in it, exiting with ELP's
  status when it is not zero.

  The project must have been described to ELP with `mix elp.build_info`:
  the description is left alone, since it may be under version control. ELP
  is `elp` on `PATH`, or the executable the `ELP` environment variable names.
  """
  @spec run([String.t()]) :: :ok
  def run(args) do
    project_dir = Path.dirname(Mix.Project.project_file())
    described?(project_dir) || not_described(project_dir)
    Mix.Task.run("compile")
    elp = System.get_env("ELP") || System.find_executable("elp") || missing_elp()

    case run_elp(elp, args, project_dir) do
      0 -> :ok
      status -> exit({:shutdown, status})
    end
  end

  @doc """
  The module name ELP knows an argument by: an Elixir alias such as `Hello`
  gets its `Elixir.` prefix, an Erlang module written as `:lists` loses its
  colon, and anything else, such as an option, passes unchanged.
  """
  @spec module_name(String.t()) :: String.t()
  def module_name(":" <> module), do: module

  def module_name(arg) do
    if arg =~ ~r/^[A-Z][A-Za-z0-9_.]*$/ and not String.starts_with?(arg, "Elixir."),
      do: "Elixir." <> arg,
      else: arg
  end

  # Whether ELP finds a description from `dir`: in it, or in a directory
  # above it, as ELP looks.
  defp described?(dir) do
    Enum.any?(@description_files, &File.exists?(Path.join(dir, &1))) or
      (Path.dirname(dir) != dir and described?(Path.dirname(dir)))
  end

  defp not_described(dir) do
    Mix.raise("""
    no build_info.json or .elp.toml in #{dir}: run `mix elp.build_info` first,
    and again after adding modules or dependencies.
    """)
  end

  defp missing_elp do
    Mix.raise("""
    elp was not found on PATH: install ELP, see
    https://whatsapp.github.io/erlang-language-platform/docs/get-started/install/,
    or set the ELP environment variable to its path.
    """)
  end

  # ELP inherits this VM's standard streams rather than writing to a port:
  # it prints for people when its output is a terminal and JSON when it is
  # not, and sees through a port only the latter.
  defp run_elp(elp, args, dir) do
    port =
      Port.open({:spawn_executable, elp}, [
        :nouse_stdio,
        :exit_status,
        args: args,
        cd: dir
      ])

    receive do
      {^port, {:exit_status, status}} -> status
    end
  end
end
