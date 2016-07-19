defmodule NimbleCSV.Mixfile do
  use Mix.Project

  def project do
    [app: :nimble_csv,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     description: "A simple and fast CSV parsing and dumping library",
     deps: deps()]
  end

  def application do
    [applications: []]
  end

  defp deps do
    []
  end
end
