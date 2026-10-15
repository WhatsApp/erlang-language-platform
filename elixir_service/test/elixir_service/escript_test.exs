defmodule ElixirService.EscriptTest do
  @moduledoc """
  Drives the built escript over the `erlang_service` stdio protocol: 4-byte
  length framing, a 3-byte tag and an 8-byte request id, then for a parse
  request the length-prefixed file text and an ETF list of path, file id and
  options. A reply carries the id, a status byte and tagged segments.
  """
  use ExUnit.Case

  @moduletag :escript

  setup do
    escript = Application.fetch_env!(:elixir_service, :escript)

    port =
      Port.open({:spawn_executable, System.find_executable("escript")}, [
        :binary,
        :exit_status,
        {:packet, 4},
        args: [escript]
      ])

    on_exit(fn -> catch_error(Port.close(port)) end)
    %{port: port}
  end

  test "parses an Elixir file from its BEAM", %{port: port} do
    fixture = Application.fetch_env!(:elixir_service, :fixture)
    ebin = Application.fetch_env!(:elixir_service, :fixture_ebin)
    options = [{:ebin, String.to_charlist(ebin)}, {:module, Hello}]
    segments = parse(port, 1, fixture, File.read!(fixture), options)
    assert {:ok, forms, []} = :erlang.binary_to_term(segments["AST"])
    assert Enum.any?(forms, &match?({:attribute, _, :module, Hello}, &1))
  end

  test "still parses an Erlang file", %{port: port} do
    text = "-module(foo).\n-export([f/0]).\nf() -> ok.\n"
    segments = parse(port, 2, "/tmp/foo.erl", text, [])
    assert {:ok, forms, []} = :erlang.binary_to_term(segments["AST"])
    assert Enum.any?(forms, &match?({:attribute, _, :module, :foo}, &1))
  end

  defp parse(port, id, path, text, options) do
    request = [
      "COM",
      <<id::64>>,
      <<byte_size(text)::32>>,
      text,
      :erlang.term_to_binary([String.to_charlist(path), 1, options])
    ]

    Port.command(port, request)
    assert_receive {^port, {:data, <<^id::64, 0, segments::binary>>}}, 30_000
    decode_segments(segments)
  end

  defp decode_segments(<<>>), do: %{}

  defp decode_segments(<<tag::binary-size(3), size::32, data::binary-size(size), rest::binary>>) do
    Map.put(decode_segments(rest), tag, data)
  end
end
