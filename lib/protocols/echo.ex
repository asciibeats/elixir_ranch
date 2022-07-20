defmodule ElixirRanch.Protocols.Echo do
  @behaviour :ranch_protocol
  @timeout 5000

  def start_link(ref, transport, opts) do
    {:ok, spawn_link(__MODULE__, :init, [ref, transport, opts])}
  end

  def init(ref, transport, _opts) do
    {:ok, socket} = :ranch.handshake(ref)
    loop(socket, transport)
  end

  defp loop(socket, transport) do
    case transport.recv(socket, 0, @timeout) do
      {:ok, data} ->
        :ok = transport.send(socket, data)
        loop(socket, transport)

      _ ->
        transport.close(socket)
    end
  end
end
