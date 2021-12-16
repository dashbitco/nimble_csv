defmodule NimbleCSVTest do
  use ExUnit.Case

  alias NimbleCSV.RFC4180, as: CSV
  alias NimbleCSV.Spreadsheet
  NimbleCSV.define(EscapedDumper, escape_formula: ?', line_separator: "\r\n")

  test "parse_string/2 without headers" do
    assert CSV.parse_string("""
           name,last,year
           john,doe,1986
           """) == [~w(john doe 1986)]
  end

  test "parse_string/2 with headers" do
    assert CSV.parse_string(
             """
             name,last,year
             john,doe,1986
             """,
             skip_headers: false
           ) == [~w(name last year), ~w(john doe 1986)]

    assert CSV.parse_string(
             """
             name,last,year
             john,doe,1986
             mary,jane,1985
             """,
             skip_headers: false
           ) == [~w(name last year), ~w(john doe 1986), ~w(mary jane 1985)]
  end

  test "parse_string/2 without trailing new line" do
    assert CSV.parse_string(
             String.trim("""
             name,last,year
             john,doe,1986
             mary,jane,1985
             """)
           ) == [~w(john doe 1986), ~w(mary jane 1985)]
  end

  test "parse_string/2 with CRLF terminations" do
    assert CSV.parse_string("name,last,year\r\njohn,doe,1986\r\n") == [~w(john doe 1986)]
  end

  test "parse_string/2 with empty string" do
    assert CSV.parse_string("", skip_headers: false) == []

    assert CSV.parse_string(
             """
             name

             john

             """,
             skip_headers: false
           ) == [["name"], [""], ["john"], [""]]
  end

  test "parse_string/2 with whitespace" do
    assert CSV.parse_string("""
           name,last,year
           \sjohn , doe , 1986\s
           """) == [[" john ", " doe ", " 1986 "]]
  end

  test "parse_string/2 with escape characters" do
    assert CSV.parse_string("""
           name,last,year
           john,"doe",1986
           """) == [~w(john doe 1986)]

    assert CSV.parse_string("""
           name,last,year
           "john",doe,"1986"
           """) == [~w(john doe 1986)]

    assert CSV.parse_string("""
           name,last,year
           "john","doe","1986"
           mary,"jane",1985
           """) == [~w(john doe 1986), ~w(mary jane 1985)]

    assert CSV.parse_string("""
           name,year
           "doe, john",1986
           "jane, mary",1985
           """) == [["doe, john", "1986"], ["jane, mary", "1985"]]
  end

  test "parse_string/2 with escape characters spawning multiple lines" do
    assert CSV.parse_string("""
           name,last,comments
           john,"doe","this is a
           really long comment
           with multiple lines"
           mary,jane,short comment
           """) == [
             ["john", "doe", "this is a\nreally long comment\nwith multiple lines"],
             ["mary", "jane", "short comment"]
           ]
  end

  test "parse_string/2 with escaped escape characters" do
    assert CSV.parse_string("""
           name,last,comments
           john,"doe","with ""double-quotes"" inside"
           mary,jane,"with , inside"
           """) == [
             ["john", "doe", "with \"double-quotes\" inside"],
             ["mary", "jane", "with , inside"]
           ]
  end

  test "parse_string/2 with invalid escape" do
    assert_raise NimbleCSV.ParseError,
                 ~s(unexpected escape character " in "john,d\\\"e,1986\\n"),
                 fn ->
                   CSV.parse_string("""
                   name,last,year
                   john,d"e,1986
                   """)
                 end

    assert_raise NimbleCSV.ParseError,
                 ~s(unexpected escape character " in "d\\\"e,1986\\n"),
                 fn ->
                   CSV.parse_string("""
                   name,last,year
                   john,"d"e,1986
                   """)
                 end

    assert_raise NimbleCSV.ParseError,
                 ~s(expected escape character " but reached the end of file),
                 fn ->
                   CSV.parse_string("""
                   name,last,year
                   john,doe,"1986
                   """)
                 end
  end

  test "parse_string/2 with encoding" do
    assert Spreadsheet.parse_string(
             utf16le("""
             name\tage
             "doe\tjohn"\t27
             jane\t28
             """)
           ) == [
             ["doe\tjohn", "27"],
             ["jane", "28"]
           ]

    assert Spreadsheet.parse_string(
             utf16le_bom() <>
               utf16le("""
               name\tage
               "doe\tjohn"\t27
               jane\t28
               """)
           ) == [
             ["doe\tjohn", "27"],
             ["jane", "28"]
           ]
  end

  test "parse_enumerable/2" do
    assert CSV.parse_enumerable([
             "name,last,year\n",
             "john,doe,1986\n"
           ]) == [~w(john doe 1986)]

    assert CSV.parse_enumerable(
             [
               "name,last,year\n",
               "john,doe,1986\n"
             ],
             skip_headers: false
           ) == [~w(name last year), ~w(john doe 1986)]

    assert_raise NimbleCSV.ParseError,
                 ~s(expected escape character " but reached the end of file),
                 fn ->
                   CSV.parse_enumerable([
                     "name,last,year\n",
                     "john,doe,\"1986\n"
                   ])
                 end

    assert Spreadsheet.parse_enumerable([
             utf16le("name\tage\n"),
             utf16le("\"doe\tjohn\"\t27\n")
           ]) == [
             ["doe\tjohn", "27"]
           ]
  end

  test "parse_stream/2" do
    stream =
      [
        "name,last,year\n",
        "john,doe,1986\n"
      ]
      |> Stream.map(&String.upcase/1)

    assert CSV.parse_stream(stream) |> Enum.to_list() == [~w(JOHN DOE 1986)]

    stream =
      [
        "name,last,year\n",
        "john,doe,1986\n"
      ]
      |> Stream.map(&String.upcase/1)

    assert CSV.parse_stream(stream, skip_headers: false) |> Enum.to_list() ==
             [~w(NAME LAST YEAR), ~w(JOHN DOE 1986)]

    stream =
      CSV.parse_stream(
        [
          "name,last,year\n",
          "john,doe,\"1986\n"
        ]
        |> Stream.map(&String.upcase/1)
      )

    assert_raise NimbleCSV.ParseError,
                 ~s(expected escape character " but reached the end of file),
                 fn ->
                   Enum.to_list(stream)
                 end

    stream =
      [
        utf16le("name\tlast\tyear\n"),
        utf16le("john\tdoe\t1986\n")
      ]
      |> Stream.map(&String.upcase/1)

    assert Spreadsheet.parse_stream(stream, skip_headers: false) |> Enum.to_list() ==
             [~w(NAME LAST YEAR), ~w(JOHN DOE 1986)]
  end

  test "dump_to_iodata/1" do
    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["john", 27]])) == """
           name,age\r\n\
           john,27\r\n\
           """

    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["john\ndoe", 27]])) == """
           name,age\r\n\
           "john
           doe",27\r\n\
           """

    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["john \"nick\" doe", 27]])) ==
             """
             name,age\r\n\
             "john ""nick"" doe",27\r\n\
             """

    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["doe, john", 27]])) == """
           name,age\r\n\
           "doe, john",27\r\n\
           """

    assert IO.iodata_to_binary(Spreadsheet.dump_to_iodata([["name", "age"], ["doe\tjohn", 27]])) ==
             utf16le_bom() <>
               utf16le("""
               name\tage
               "doe\tjohn"\t27
               """)

    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["goku", "=SUM(5,5)"]])) ==
             """
             name,age\r\n\
             goku,\"=SUM(5,5)\"\r\n\
             """

    assert IO.iodata_to_binary(
             EscapedDumper.dump_to_iodata([["name", "age"], ["goku", "=SUM(5,5)"]])
           ) == """
           name,age\r\n\
           goku,\"'=SUM(5,5)\"\r\n\
           """
  end

  test "dump_to_stream/1" do
    assert IO.iodata_to_binary(Enum.to_list(CSV.dump_to_stream([["name", "age"], ["john", 27]]))) ==
             """
             name,age\r\n\
             john,27\r\n\
             """

    assert IO.iodata_to_binary(
             Enum.to_list(CSV.dump_to_stream([["name", "age"], ["john\ndoe", 27]]))
           ) == """
           name,age\r\n\
           "john
           doe",27\r\n\
           """

    assert IO.iodata_to_binary(
             Enum.to_list(CSV.dump_to_stream([["name", "age"], ["john \"nick\" doe", 27]]))
           ) == """
           name,age\r\n\
           "john ""nick"" doe",27\r\n\
           """

    assert IO.iodata_to_binary(
             Enum.to_list(Spreadsheet.dump_to_stream([["name", "age"], ["john\tnick", 27]]))
           ) ==
             utf16le_bom() <>
               utf16le("""
               name\tage
               "john\tnick"\t27
               """)
  end

  describe "multiple separators" do
    NimbleCSV.define(CSVWithUnknownSeparator, separator: [",", ";", "\t"])

    test "parse_string/2 (unknown separator)" do
      assert CSVWithUnknownSeparator.parse_string("""
             name,last\tyear
             john;doe,1986
             """) == [~w(john doe 1986)]
    end

    test "parse_stream/2 (unknown separator)" do
      stream =
        [
          "name,last\tyear\n",
          "john;doe,1986\n"
        ]
        |> Stream.map(&String.upcase/1)

      assert CSVWithUnknownSeparator.parse_stream(stream) |> Enum.to_list() == [~w(JOHN DOE 1986)]

      stream =
        [
          "name,last\tyear\n",
          "john;doe,1986\n"
        ]
        |> Stream.map(&String.upcase/1)

      assert CSVWithUnknownSeparator.parse_stream(stream, skip_headers: false) |> Enum.to_list() ==
               [~w(NAME LAST YEAR), ~w(JOHN DOE 1986)]

      stream =
        CSVWithUnknownSeparator.parse_stream(
          [
            "name,last\tyear\n",
            "john;doe,\"1986\n"
          ]
          |> Stream.map(&String.upcase/1)
        )

      assert_raise NimbleCSV.ParseError,
                   ~s(expected escape character " but reached the end of file),
                   fn ->
                     Enum.to_list(stream)
                   end
    end

    test "dump_to_iodata/1 (unknown separator)" do
      assert IO.iodata_to_binary(
               CSVWithUnknownSeparator.dump_to_iodata([["name", "age"], ["john", 27]])
             ) == """
             name,age
             john,27
             """

      assert IO.iodata_to_binary(
               CSVWithUnknownSeparator.dump_to_iodata([["name", "age"], ["john\ndoe", 27]])
             ) == """
             name,age
             "john
             doe",27
             """

      assert IO.iodata_to_binary(
               CSVWithUnknownSeparator.dump_to_iodata([["name", "age"], ["john \"nick\" doe", 27]])
             ) == """
             name,age
             "john ""nick"" doe",27
             """

      assert IO.iodata_to_binary(
               CSVWithUnknownSeparator.dump_to_iodata([["name", "age"], ["doe, john", 27]])
             ) == """
             name,age
             "doe, john",27
             """
    end

    test "dump_to_stream/1 (unknown separator)" do
      assert IO.iodata_to_binary(
               Enum.to_list(
                 CSVWithUnknownSeparator.dump_to_stream([["name", "age"], ["john", 27]])
               )
             ) == """
             name,age
             john,27
             """

      assert IO.iodata_to_binary(
               Enum.to_list(
                 CSVWithUnknownSeparator.dump_to_stream([["name", "age"], ["john\ndoe", 27]])
               )
             ) == """
             name,age
             "john
             doe",27
             """

      assert IO.iodata_to_binary(
               Enum.to_list(
                 CSVWithUnknownSeparator.dump_to_stream([
                   ["name", "age"],
                   ["john \"nick\" doe", 27]
                 ])
               )
             ) == """
             name,age
             "john ""nick"" doe",27
             """
    end

    test "parse_string/2 with escape characters (unknown separator)" do
      assert CSV.parse_string("""
             name,year
             "doe, john",1986
             "jane; mary",1985
             """) == [["doe, john", "1986"], ["jane; mary", "1985"]]
    end
  end

  test "to_line_stream/2" do
    stream = [
      "name,last,year\n",
      "john,doe,1986\n",
      "jane,",
      "doe,1987\n",
      "james,doe,1992\nryan,doe",
      ",1893"
    ]

    assert [
             "name,last,year\n",
             "john,doe,1986\n",
             "jane,doe,1987\n",
             "james,doe,1992\n",
             "ryan,doe,1893"
           ] = CSV.to_line_stream(stream) |> Enum.into([])

    assert [~w(john doe 1986) | _] =
             CSV.to_line_stream(stream) |> CSV.parse_stream() |> Enum.to_list()

    stream =
      [
        "name\tlast\tyear\n",
        "john\tdoe\t1986\n",
        "jane\t",
        "doe\t1987\n",
        "james\tdoe\t1992\nryan\tdoe",
        "\t1893"
      ]
      |> Stream.map(&utf16le/1)

    assert [~w(john doe 1986) | _] =
             Spreadsheet.to_line_stream(stream) |> Spreadsheet.parse_stream() |> Enum.to_list()
  end

  defp utf16le(binary), do: :unicode.characters_to_binary(binary, :utf8, {:utf16, :little})
  defp utf16le_bom(), do: :unicode.encoding_to_bom({:utf16, :little})
end
