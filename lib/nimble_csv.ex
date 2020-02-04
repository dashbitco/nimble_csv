defmodule NimbleCSV do
  @moduledoc ~S"""
  NimbleCSV is a small and fast parsing and dumping library.

  It works by building highly-inlined CSV parsers, designed
  to work with strings, enumerables and streams. At the top
  of your file (and not inside a function), you can define your
  own parser module:

      NimbleCSV.define(MyParser, separator: "\t", escape: "\"")

  Once defined, we can parse data accordingly:

      iex> MyParser.parse_string "name\tage\njohn\t27"
      [["john","27"]]

  See the `define/2` function for the list of functions that
  would be defined in `MyParser`.

  ## Parsing

  NimbleCSV is by definition restricted in scope to do only
  parsing (and dumping). For example, the example above
  discarded the headers when parsing the string, as NimbleCSV
  expects developers to handle those explicitly later.
  For example:

      "name\tage\njohn\t27"
      |> MyParser.parse_string
      |> Enum.map(fn [name, age] ->
        %{name: name, age: String.to_integer(age)}
      end)

  This is particularly useful with the parse_stream functionality
  that receives and returns a stream. For example, we can use it
  to parse files line by line lazily:

      "path/to/csv/file"
      |> File.stream!(read_ahead: 100_000)
      |> MyParser.parse_stream
      |> Stream.map(fn [name, age] ->
        %{name: name, age: String.to_integer(age)}
      end)

  By default this library ships with two implementations:

    * `NimbleCSV.RFC4180`, which is the most common implementation of
      CSV parsing/dumping available using comma as separators and
      double-quote as escape. If you want to use it in your codebase,
      simply alias it to CSV and enjoy:

          iex> alias NimbleCSV.RFC4180, as: CSV
          iex> CSV.parse_string "name,age\njohn,27"
          [["john","27"]]

    * `NimbleCSV.Spreadsheet`, which uses UTF-16 and is most commonly
      used by spreadsheet software, such as Excel, Numbers, etc.

  ### Binary references

  One of the reasons behind NimbleCSV performance is that it performs
  parsing by matching on binaries and extracting those fields as
  binary references. Therefore if you have a row such as:

      one,two,three,four,five

  NimbleCSV will return a list of `["one", "two", "three", "four", "five"]`
  where each element references the original row. For this reason, if
  you plan to keep the parsed data around in the parsing process or even
  send it to another process, you may want to copy the data before doing
  the transfer.

  For example, in the `parse_stream` example in the previous section,
  we could rewrite the `Stream.map/2` operation to explicitly copy any
  field that is stored as a binary:

      "path/to/csv/file"
      |> File.stream!(read_ahead: 100_000)
      |> MyParser.parse_stream
      |> Stream.map(fn [name, age] ->
        %{name: :binary.copy(name),
          age: String.to_integer(age)}
      end)

  ## Dumping

  NimbleCSV can dump any enumerable to either iodata or to streams:

      iex> IO.iodata_to_binary MyParser.dump_to_iodata([~w(name age), ~w(mary 28)])
      "name\tage\nmary\t28\n"

      iex> MyParser.dump_to_stream([~w(name age), ~w(mary 28)])
      #Stream<...>

  """

  defmodule ParseError do
    defexception [:message]
  end

  @doc """
  Eagerly dumps an enumerable into iodata (a list of binaries and bytes and other lists).
  """
  @callback dump_to_iodata(rows :: Enumerable.t()) :: iodata()

  @doc """
  Lazily dumps from an enumerable to a stream.

  It returns a stream that emits each row as iodata.
  """
  @callback dump_to_stream(rows :: Enumerable.t()) :: Enumerable.t()

  @doc """
  Same as `parse_enumerable(enumerable, [])`.
  """
  @callback parse_enumerable(enum :: Enumerable.t()) :: [[binary()]]

  @doc """
  Eagerly parses CSV from an enumerable and returns a list of rows.

  ## Options

    * `:skip_headers` - when `true`, skips headers. Defaults to `true`.
      Set it to false to keep headers or when the CSV has no headers.

  """
  @callback parse_enumerable(enum :: Enumerable.t(), opts :: keyword()) :: [[binary()]]

  @doc """
  Same as `parse_stream(enumerable, [])`.
  """
  @callback parse_stream(enum :: Enumerable.t()) :: Enumerable.t()

  @doc """
  Lazily parses CSV from a stream and returns a stream of rows.

  ## Options

    * `:skip_headers` - when `true`, skips headers. Defaults to `true`.
      Set it to false to keep headers or when the CSV has no headers.

  """
  @callback parse_stream(enum :: Enumerable.t(), opts :: keyword()) :: Enumerable.t()

  @doc """
  Same as `parse_string(enumerable, [])`.
  """
  @callback parse_string(binary()) :: [[binary()]]

  @doc """
  Eagerly parses CSV from a string and returns a list of rows.

  ## Options

    * `:skip_headers` - when `true`, skips headers. Defaults to `true`.
      Set it to false to keep headers or when the CSV has no headers.

  """
  @callback parse_string(binary(), opts :: keyword()) :: [[binary()]]

  @doc ~S"""
  Defines a new parser/dumper.

  Calling this function defines a CSV module. Therefore, `define`
  is typically invoked at the top of your files and not inside
  functions. Placing it inside a function would cause the same
  module to be defined multiple times, one time per invocation,
  leading your code to emit warnings and slowing down execution.

  It accepts the following options:

    * `:moduledoc` - the documentation for the generated module

  The following options control parsing:

    * `:escape`- the CSV escape, defaults to `"\""`
    * `:encoding` - converts the given data from encoding to UTF-8
    * `:separator`- the CSV separators, defaults to `","`. It can be
      a string or a list of strings. If a list is given, the first entry
      is used for dumping (see below)
    * `:newlines` - the list of entries to be considered newlines
      when parsing, defaults to `["\r\n", "\n"]` (note they are attempted
      in order, so the order matters)
    * `:trim_bom` - automatically trims BOM (byte-order marker) when parsing
      string. Note the bom is not trimmed for enumerables or streams. In such
      cases, the BOM must be trimmed directly in the stream, such as
      `File.stream!(path, [:trim_bom])`

  The following options control dumping:

    * `:escape`- the CSV escape character, defaults to `"\""`
    * `:encoding` - converts the given data from UTF-8 to the given encoding
    * `:separator`- the CSV separator character, defaults to `","`
    * `:line_separator` - the CSV line separator character, defaults to `"\n"`
    * `:dump_bom` - includes BOM (byte order marker) in the dumped document
    * `:reserved` - the list of characters to be escaped, it defaults to the
      `:separator`, `:line_separator` and `:escape` characters above.

  Although parsing may support multiple newline delimiters, when
  dumping only one of them must be picked, which is controlled by
  the `:line_separator` option. This allows NimbleCSV to handle both
  `"\r\n"` and `"\n"` when parsing, but only the latter for dumping.

  ## Parser/Dumper API

  Modules defined with `define/2` implement the `NimbleCSV` behaviour. See
  the callbacks for this behaviour for information on the generated functions
  and their documentation.
  """
  def define(module, options) do
    defmodule module do
      @behaviour NimbleCSV
      @moduledoc Keyword.get(options, :moduledoc)

      @escape Keyword.get(options, :escape, "\"")
      @separator (case Keyword.get(options, :separator, ",") do
                    many when is_list(many) -> many
                    one when is_binary(one) -> [one]
                  end)
      @line_separator Keyword.get(options, :line_separator, "\n")
      @newlines Keyword.get(options, :newlines, ["\r\n", "\n"])
      @reserved Keyword.get(options, :reserved, [@escape, @line_separator | @separator])

      # BOM and Encoding related

      encoding = Keyword.get(options, :encoding, :utf8)
      @bom :unicode.encoding_to_bom(encoding)
      @encoding encoding
      @encoded_newlines Enum.map(@newlines, &:unicode.characters_to_binary(&1, :utf8, encoding))

      if Keyword.get(options, :trim_bom, false) do
        defp maybe_trim_bom(@bom <> string), do: string
        defp maybe_trim_bom(string), do: string
      else
        defp maybe_trim_bom(string), do: string
      end

      if Keyword.get(options, :dump_bom, false) do
        defp maybe_dump_bom(list) when is_list(list), do: [@bom | list]
        defp maybe_dump_bom(stream), do: Stream.concat([@bom], stream)
      else
        defp maybe_dump_bom(data), do: data
      end

      if encoding == :utf8 do
        defp maybe_to_utf8(line), do: line
        defp maybe_to_encoding(line), do: line
      else
        defp maybe_to_utf8(line) do
          case :unicode.characters_to_binary(line, @encoding, :utf8) do
            binary when is_binary(binary) ->
              binary

            reason ->
              raise "error converting #{inspect(@encoding)} to :utf8, got: #{inspect(reason)}"
          end
        end

        defp maybe_to_encoding(line) do
          case :unicode.characters_to_binary(line, :utf8, @encoding) do
            binary when is_binary(binary) ->
              binary

            reason ->
              raise "error converting :utf8 to #{inspect(@encoding)}, got: #{inspect(reason)}"
          end
        end
      end

      _ = @bom
      _ = @encoding

      @compile {:inline,
                maybe_dump_bom: 1, maybe_trim_bom: 1, maybe_to_utf8: 1, maybe_to_encoding: 1}

      ## Parser

      def parse_stream(stream, opts \\ []) when is_list(opts) do
        {state, separator, escape} = init_parser(opts)

        Stream.transform(
          stream,
          fn -> state end,
          &parse(maybe_to_utf8(&1), &2, separator, escape),
          &finalize_parser/1
        )
      end

      def parse_enumerable(enumerable, opts \\ []) when is_list(opts) do
        {state, separator, escape} = init_parser(opts)

        {lines, state} =
          Enum.flat_map_reduce(
            enumerable,
            state,
            &parse(maybe_to_utf8(&1), &2, separator, escape)
          )

        finalize_parser(state)
        lines
      end

      def parse_string(string, opts \\ []) when is_binary(string) and is_list(opts) do
        newline = :binary.compile_pattern(@encoded_newlines)
        string = string |> maybe_trim_bom()

        {0, byte_size(string)}
        |> Stream.unfold(fn
          {_, 0} ->
            nil

          {offset, length} ->
            case :binary.match(string, newline, scope: {offset, length}) do
              {newline_offset, newline_length} ->
                difference = newline_length + newline_offset - offset

                {binary_part(string, offset, difference),
                 {newline_offset + newline_length, length - difference}}

              :nomatch ->
                {binary_part(string, offset, length), {offset + length, 0}}
            end
        end)
        |> parse_enumerable(opts)
      end

      defp init_parser(opts) do
        state =
          if Keyword.has_key?(opts, :headers) do
            IO.warn("the :headers option is deprecated, please use :skip_headers instead")
            if Keyword.get(opts, :headers, true), do: :header, else: :line
          else
            if Keyword.get(opts, :skip_headers, true), do: :header, else: :line
          end

        {state, :binary.compile_pattern(@separator), :binary.compile_pattern(@escape)}
      end

      defp finalize_parser({:escape, _, _, _}) do
        raise ParseError, "expected escape character #{@escape} but reached the end of file"
      end

      defp finalize_parser(_) do
        :ok
      end

      defp to_enum(result) do
        case result do
          {:line, row} -> {[row], :line}
          {:header, _} -> {[], :line}
          {:escape, _, _, _} = escape -> {[], escape}
        end
      end

      defp parse(line, {:escape, entry, row, state}, separator, escape) do
        to_enum(escape(line, entry, row, state, separator, escape))
      end

      defp parse(line, state, separator, escape) do
        to_enum(separator(line, [], state, separator, escape))
      end

      defmacrop newlines_separator!() do
        newlines_offsets =
          for {newline, i} <- Enum.with_index(@newlines) do
            quote do
              unquote(Macro.var(:"count#{i}", Elixir)) = offset - unquote(byte_size(newline))
            end
          end

        newlines_clauses =
          @newlines
          |> Enum.with_index()
          |> Enum.flat_map(fn {newline, i} ->
            quote do
              <<prefix::size(unquote(Macro.var(:"count#{i}", Elixir)))-binary, unquote(newline)>> ->
                prefix
            end
          end)
          |> Kernel.++(quote do: (prefix -> prefix))

        quote do
          offset = byte_size(var!(line))
          unquote(newlines_offsets)
          case var!(line), do: unquote(newlines_clauses)
        end
      end

      defmacrop separator_case() do
        clauses =
          Enum.flat_map(@separator, fn sep ->
            quote do
              <<prefix::size(var!(pos))-binary, unquote(sep), @escape, rest::binary>> ->
                escape(
                  rest,
                  "",
                  var!(row) ++ :binary.split(prefix, var!(separator), [:global]),
                  var!(state),
                  var!(separator),
                  var!(escape)
                )
            end
          end)

        catch_all =
          quote do
            _ ->
              raise ParseError,
                    "unexpected escape character #{@escape} in #{inspect(var!(line))}"
          end

        quote do
          case var!(line) do
            unquote(clauses ++ catch_all)
          end
        end
      end

      defp separator(line, row, state, separator, escape) do
        case :binary.match(line, escape) do
          {0, _} ->
            <<@escape, rest::binary>> = line
            escape(rest, "", row, state, separator, escape)

          {pos, _} ->
            pos = pos - 1
            separator_case()

          :nomatch ->
            pruned = newlines_separator!()
            {state, row ++ :binary.split(pruned, separator, [:global])}
        end
      end

      defmacrop newlines_escape!(match) do
        newlines_before =
          quote do
            <<prefix::size(offset)-binary, @escape, @escape, rest::binary>> ->
              escape(
                rest,
                var!(entry) <> prefix <> <<@escape>>,
                var!(row),
                var!(state),
                var!(separator),
                var!(escape)
              )
          end ++
            Enum.flat_map(@separator, fn sep ->
              quote do
                <<prefix::size(offset)-binary, @escape, unquote(sep), rest::binary>> ->
                  separator(
                    rest,
                    var!(row) ++ [var!(entry) <> prefix],
                    var!(state),
                    var!(separator),
                    var!(escape)
                  )
              end
            end)

        newlines_clauses =
          Enum.flat_map(@newlines, fn newline ->
            quote do
              <<prefix::size(offset)-binary, @escape, unquote(newline)>> ->
                {var!(state), var!(row) ++ [var!(entry) <> prefix]}
            end
          end)

        newlines_after =
          quote do
            <<prefix::size(offset)-binary, @escape>> ->
              {var!(state), var!(row) ++ [var!(entry) <> prefix]}

            _ ->
              raise ParseError, "unexpected escape character #{@escape} in #{inspect(var!(line))}"
          end

        quote do
          case unquote(match) do
            {offset, _} ->
              case var!(line), do: unquote(newlines_before ++ newlines_clauses ++ newlines_after)

            :nomatch ->
              {:escape, var!(entry) <> var!(line), var!(row), var!(state)}
          end
        end
      end

      defp escape(line, entry, row, state, separator, escape) do
        newlines_escape!(:binary.match(line, escape))
      end

      @compile {:inline, init_parser: 1, to_enum: 1, parse: 4}

      ## Dumper

      def dump_to_iodata(enumerable) do
        check = init_dumper()

        enumerable
        |> Enum.map(&dump(&1, check))
        |> maybe_dump_bom()
      end

      def dump_to_stream(enumerable) do
        check = init_dumper()

        enumerable
        |> Stream.map(&dump(&1, check))
        |> maybe_dump_bom()
      end

      @encoded_escape (case @escape
                            |> :unicode.characters_to_binary(:utf8, encoding) do
                         <<x>> -> x
                         x -> x
                       end)

      @encoded_separator (case @separator
                               |> hd()
                               |> :unicode.characters_to_binary(:utf8, encoding) do
                            <<x>> -> x
                            x -> x
                          end)

      @encoded_line_separator (case @line_separator
                                    |> :unicode.characters_to_binary(:utf8, encoding) do
                                 <<x>> -> x
                                 x -> x
                               end)

      @replacement @escape <> @escape

      defp init_dumper() do
        :binary.compile_pattern(@reserved)
      end

      defp dump([], _check) do
        [@encoded_line_separator]
      end

      defp dump([entry], check) do
        [maybe_escape(entry, check), @encoded_line_separator]
      end

      defp dump([entry | entries], check) do
        [maybe_escape(entry, check), @encoded_separator | dump(entries, check)]
      end

      defp maybe_escape(entry, check) do
        entry = to_string(entry)

        case :binary.match(entry, check) do
          {_, _} ->
            replaced = :binary.replace(entry, @escape, @replacement, [:global])
            [@encoded_escape, maybe_to_encoding(replaced), @encoded_escape]

          :nomatch ->
            maybe_to_encoding(entry)
        end
      end

      @compile {:inline, init_dumper: 0, maybe_escape: 2}
    end
  end
end

NimbleCSV.define(NimbleCSV.RFC4180,
  separator: ",",
  escape: "\"",
  moduledoc: """
  A CSV parser that uses comma as separator and double-quotes as escape according to RFC4180.
  """
)

NimbleCSV.define(NimbleCSV.Spreadsheet,
  separator: "\t",
  escape: "\"",
  encoding: {:utf16, :little},
  trim_bom: true,
  dump_bom: true,
  moduledoc: """
  A parser with spreadsheet friendly settings.

  The parser uses tab as separator and double-quotes as escape, as required by
  common spreadsheet softwtare such as Excel, Numbers and OpenOffice. It's encoded
  in UTF-16 little-endian with a byte-order BOM.
  """
)
