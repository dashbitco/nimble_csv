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

## License

Copyright 2016 Plataformatec

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.