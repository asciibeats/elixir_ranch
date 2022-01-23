defmodule ElixirRanch.Listeners.Echo do
  def child_spec(opts) do
    :ranch.child_spec(__MODULE__, :ranch_tcp, opts, ElixirRanch.Protocols.Echo, [])
  end
end
