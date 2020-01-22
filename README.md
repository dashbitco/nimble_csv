# NimbleCSV [![Hex Version](https://img.shields.io/hexpm/v/nimble_csv.svg)](https://hex.pm/packages/nimble_csv) [![docs](https://img.shields.io/badge/docs-hexpm-blue.svg)](https://hexdocs.pm/nimble_csv/)

NimbleCSV is a simple and extremely fast CSV parsing/dumping library for Elixir. Its performance comes from allowing developers to define custom parsers/dumpers which rely on binary patterns for efficiency:

```elixir
# Define the parser (this is equivalent to calling
# defmodule and should be done at the top of a file)
NimbleCSV.define(MyParser, separator: "\t", escape: "\"")

# Parse the data
MyParser.parse_string "name\tage\njohn\t27"
#=> [["john","27"]]
```

NimbleCSV provides both eager and lazy (streaming) parsing as well as data dumping. The library was designed to be simple and compose well. For example, instead of trying to tackle how data is cast or how headers are managed, it allows the developers to build on top of the parsed results:

```elixir
# Lazily parses a file stream
"path/to/file"
|> File.stream!
|> MyParser.parse_stream
|> Stream.map(fn [name, age] ->
  %{name: name, age: String.to_integer(age)}
end)
```

By default this library ships with `NimbleCSV.RFC4180`, which is the most common implementation of CSV parsing/dumping available using comma as separators and double-quote as escape. If you want to use it in your codebase, simply alias it to CSV and enjoy:

```elixir
alias NimbleCSV.RFC4180, as: CSV
CSV.parse_string "name,age\njohn,27"
[["john","27"]]
```

See the [online documentation](https://hexdocs.pm/nimble_csv) for more information.

## Installation

  1. Add `nimble_csv` to your list of dependencies in `mix.exs`:

        ```elixir
        def deps do
          [{:nimble_csv, "~> 0.6"}]
        end
        ```

  2. If using Elixir < 1.4, ensure `nimble_csv` is started before your application:

        ```elixir
        def application do
          [applications: [:nimble_csv]]
        end
        ```

## Nimble*

Other nimble libraries by Dashbit:

  * [NimbleParsec](https://github.com/dashbitco/nimble_parsec) - simple and fast parser combinators
  * [NimblePool](https://github.com/dashbitco/nimble_pool) - simple and small resource-pool

## License

Copyright 2016 Plataformatec \
Copyright 2020 Dashbit

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
