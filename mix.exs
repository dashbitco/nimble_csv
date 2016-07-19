defmodule NimbleCSV.Mixfile do
  use Mix.Project

  @version "0.1.0"

  def project do
    [app: :nimble_csv,
     version: @version,
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     name: "NimbleCSV",
     description: "A simple and fast CSV parsing and dumping library",
     deps: deps(),
     docs: docs(),
     package: package()]
  end

  def application do
    [applications: []]
  end

  defp deps do
    [{:ex_doc, "~> 0.13", only: :docs}]
  end

  defp docs do
    [main: "NimbleCSV", source_ref: "v#{@version}",
     source_url: "https://github.com/plataformatec/nimble_csv"]
  end

  defp package do
    %{licenses: ["Apache 2"],
      links: %{"GitHub" => "https://github.com/plataformatec/nimble_csv"}}
  end
end
