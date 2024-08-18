defmodule NimbleCSV.Mixfile do
  use Mix.Project

  @source_url "https://github.com/dashbitco/nimble_csv"
  @version "1.2.0"

  def project do
    [
      app: :nimble_csv,
      version: @version,
      elixir: "~> 1.6",
      name: "NimbleCSV",
      description: "A simple and fast CSV parsing and dumping library",
      deps: deps(),
      docs: docs(),
      package: package()
    ]
  end

  defp deps do
    [{:ex_doc, "~> 0.18", only: :docs}, {:benchee, "~> 1.0", only: :dev}]
  end

  defp docs do
    [
      main: "NimbleCSV",
      source_ref: "v#{@version}",
      source_url: @source_url
    ]
  end

  defp package do
    [
      licenses: ["Apache-2.0"],
      maintainers: ["José Valim"],
      links: %{
        "Changelog" => "#{@source_url}/blob/master/CHANGELOG.md",
        "GitHub" => @source_url
      }
    ]
  end
end
