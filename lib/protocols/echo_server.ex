defmodule ElixirRanch.Protocols.EchoServer do
  use GenServer
  @behaviour :ranch_protocol
  @timeout 5000

  @impl true
  def start_link(ref, transport, opts) do
	  {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
  end

  @impl true
  def init({ref, transport, _opts}) do
	  {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    :gen_server.enter_loop(__MODULE__, [], {socket, transport}, @timeout)
  end

  @impl true
  def handle_info({:tcp, socket, data}, {socket, transport} = state) do
    transport.send(socket, data)
    transport.setopts(socket, active: :once)
    {:noreply, state, @timeout}
  end

  @impl true
  def handle_info(_, {socket, transport} = state) do
    transport.close(socket)
    {:stop, :shutdown, state}
  end
end
