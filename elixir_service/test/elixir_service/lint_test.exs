defmodule ElixirService.LintTest do
  use ExUnit.Case, async: true

  alias ElixirService.Lint

  setup_all do
    fixture = Application.fetch_env!(:elixir_service, :fixture)
    ebin = Application.fetch_env!(:elixir_service, :fixture_ebin)
    %{fixture: fixture, ebin: String.to_charlist(ebin), text: File.read!(fixture)}
  end

  test "serves the forms of an .ex file from its BEAM", ctx do
    options = [{:ebin, ctx.ebin}, {:elp_metadata, [{:eqwalizer_fixmes, []}]}]
    assert {:ok, [{"AST", ast}]} = run(ctx, options)
    assert {:ok, forms, []} = :erlang.binary_to_term(IO.iodata_to_binary(ast))
    assert Enum.any?(forms, &match?({:attribute, _, :module, Hello}, &1))
    assert Enum.any?(forms, &match?({:attribute, _, :elp_metadata, _}, &1))
    assert Enum.any?(forms, &match?({:attribute, _, :spec, {{:add, 2}, _}}, &1))
  end

  test "serves the module asked for when a file defines several", ctx do
    assert {:ok, [{"AST", ast}]} = run(ctx, [{:ebin, ctx.ebin}, {:module, Hello.Nested}])
    assert {:ok, forms, []} = :erlang.binary_to_term(IO.iodata_to_binary(ast))
    assert Enum.any?(forms, &match?({:attribute, _, :module, Hello.Nested}, &1))
    refute Enum.any?(forms, &match?({:attribute, _, :module, Hello}, &1))
  end

  test "drops the functions the compiler generated but keeps their specs and exports", ctx do
    assert {:ok, [{"AST", ast}]} = run(ctx, [{:ebin, ctx.ebin}, {:module, Hello}])
    assert {:ok, forms, []} = :erlang.binary_to_term(IO.iodata_to_binary(ast))
    refute Enum.any?(forms, &match?({:function, _, :__info__, 1, _}, &1))
    refute Enum.any?(forms, &match?({:function, _, :__struct__, _, _}, &1))
    assert Enum.any?(forms, &match?({:function, _, :add, 2, _}, &1))
    assert Enum.any?(forms, &match?({:attribute, _, :spec, {{:__info__, 1}, _}}, &1))

    assert Enum.any?(forms, fn
             {:attribute, _, :export, funs} -> {:__struct__, 0} in funs
             _ -> false
           end)
  end

  test "serves a module from its BEAM file, without positions", ctx do
    beam = Path.join(ctx.ebin, "Elixir.Hello.beam")
    assert {:ok, [{"AST", ast}]} = Lint.run(1, [beam, 1, [], "", &post_process/2, true])
    assert {:ok, forms, []} = :erlang.binary_to_term(IO.iodata_to_binary(ast))
    assert Enum.any?(forms, &match?({:attribute, _, :module, Hello}, &1))

    assert {:function, {0, 0}, :add, 2, _} =
             Enum.find(forms, &match?({:function, _, :add, 2, _}, &1))
  end

  test "reports a request without an ebin directory", ctx do
    assert {:error, message} = run(ctx, [])
    assert message =~ "no ebin directory"
  end

  test "reports a module without a BEAM file", ctx do
    assert {:error, message} = run(ctx, [{:ebin, ctx.ebin}, {:module, Hello.Missing}])
    assert message =~ "BEAM file not found"
  end

  defp run(ctx, options) do
    Lint.run(1, [String.to_charlist(ctx.fixture), 1, options, ctx.text, &post_process/2, true])
  end

  defp post_process(forms, _file_name), do: :erlang.term_to_binary({:ok, forms, []})
end
