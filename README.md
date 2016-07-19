# NimbleCSV

NimbleCSV is a simple and extremely fast library CSV parsing library for Elixir. It does only the minimum necessary and allow developers to add their domain complexities as necessary. It supports for eager and lazy (streaming) modes.

See the [online documentation](https://hexdocs.pm/nimble_csv).

## Installation


  1. Add `nimble_csv` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:nimble_csv, "~> 0.1.0"}]
    end
    ```

  2. Ensure `nimble_csv` is started before your application:

    ```elixir
    def application do
      [applications: [:nimble_csv]]
    end
    ```
