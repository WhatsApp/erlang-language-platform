defmodule ElixirService.OverlayTest do
  @moduledoc """
  The eqWAlizer overrides for the Elixir standard library in
  `priv/eqwalizer_support`, and the dependency `mix elp.build_info` ships
  them as.
  """
  use ExUnit.Case, async: true

  @overlay Path.expand("../../priv/eqwalizer_support/src", __DIR__)

  test "every spec overrides a 'Module:function' and has a stub clause" do
    forms = parse("eqwalizer_specs.erl")
    specs = for {:attribute, _, :spec, {{name, arity}, _}} <- forms, do: {name, arity}
    funs = for {:function, _, name, arity, _} <- forms, do: {name, arity}
    assert specs != []
    assert Enum.sort(specs) == Enum.sort(funs)

    for {name, _arity} <- specs do
      assert Atom.to_string(name) =~ ~r/^Elixir\.[A-Za-z.]+:[a-z_?!]+$/
    end
  end

  test "every type override is exported and named 'Module:type'" do
    forms = parse("eqwalizer_types.erl")
    types = for {:attribute, _, :type, {name, _body, args}} <- forms, do: {name, length(args)}
    exported = for {:attribute, _, :export_type, ids} <- forms, id <- ids, do: id
    assert types != []
    assert Enum.sort(types) == Enum.sort(exported)

    for {name, _arity} <- types do
      assert Atom.to_string(name) =~ ~r/^Elixir\.[A-Za-z.]+:[a-z_]+$/
    end
  end

  @tag :tmp_dir
  @tag timeout: 120_000
  test "the build info lists the overlay as a dependency of Erlang sources", %{tmp_dir: tmp} do
    {_, 0} = System.cmd("mix", ["new", "overlay_probe"], cd: tmp, stderr_to_stdout: true)
    project = Path.join(tmp, "overlay_probe")
    env = [{"MIX_ENV", Atom.to_string(Mix.env())}]
    {_, 0} = System.cmd("mix", ["compile"], cd: project, env: env, stderr_to_stdout: true)

    output = Path.join(tmp, "build_info.json")
    Mix.Tasks.Elp.BuildInfo.run(["--project", project, "--output", output, "--no-elp-toml"])
    %{"apps" => [app], "deps" => deps} = JSON.decode!(File.read!(output))

    assert app["name"] == "overlay_probe"
    assert app["modules"] == %{"Elixir.OverlayProbe" => "lib/overlay_probe.ex"}

    overlay = Enum.find(deps, &(&1["name"] == "elixir_eqwalizer_support"))
    assert overlay["src_dirs"] == ["src"]
    refute Map.has_key?(overlay, "ebin")
    overlay_dir = Path.expand(overlay["dir"], tmp)
    assert File.regular?(Path.join(overlay_dir, "src/eqwalizer_specs.erl"))
    assert File.regular?(Path.join(overlay_dir, "src/eqwalizer_types.erl"))

    Mix.Tasks.Elp.BuildInfo.run([
      "--project",
      project,
      "--output",
      output,
      "--no-elp-toml",
      "--no-overlay"
    ])

    %{"deps" => deps} = JSON.decode!(File.read!(output))
    refute Enum.any?(deps, &(&1["name"] == "elixir_eqwalizer_support"))
  end

  defp parse(file) do
    {:ok, forms} = :epp.parse_file(String.to_charlist(Path.join(@overlay, file)), [])
    assert [] == for({:error, error} <- forms, do: error)
    forms
  end
end
