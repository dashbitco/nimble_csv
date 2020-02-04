defmodule NimbleCSV.Mixfile do
  use Mix.Project

  @version "0.7.0"

  def project do
    [
      app: :nimble_csv,
      version: @version,
      elixir: "~> 1.3",
      name: "NimbleCSV",
      description: "A simple and fast CSV parsing and dumping library",
      deps: deps(),
      docs: docs(),
      package: package()
    ]
  end

  def application do
    [applications: []]
  end

  defp deps do
    [{:ex_doc, "~> 0.18", only: :docs}]
  end

  defp docs do
    [
      main: "NimbleCSV",
      source_ref: "v#{@version}",
      source_url: "https://github.com/dashbitco/nimble_csv"
    ]
  end

  defp package do
    %{
      licenses: ["Apache 2"],
      maintainers: ["José Valim"],
      links: %{"GitHub" => "https://github.com/dashbitco/nimble_csv"}
    }
  end
end
