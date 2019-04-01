defmodule NimbleCSVTest do
  use ExUnit.Case

  alias NimbleCSV.RFC4180, as: CSV

  test "parse_string/2" do
    assert CSV.parse_string("""
           name,last,year
           john,doe,1986
           """) == [~w(john doe 1986)]
  end

  test "parse_string/2 without headers" do
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
             string_trim("""
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
    assert %NimbleCSV.ParseError{
             message: ~s(unexpected escape character " in "john,d\\\"e,1986\\n"),
             line_number: 2
           } ==
             catch_error(
               CSV.parse_string("""
               name,last,year
               john,d"e,1986
               """)
             )

    assert %NimbleCSV.ParseError{
             message: ~s(unexpected escape character " in "d\\\"e,1986\\n"),
             line_number: 4
           } ==
             catch_error(
               CSV.parse_string("""
               name,last,year
               1,2,3
               4,5,6
               john,"d"e,1986
               """)
             )

    assert %NimbleCSV.ParseError{
             message: ~s(unexpected escape character " in "la\\\"st,year\\n"),
             line_number: 1
           } ==
             catch_error(
               CSV.parse_string("""
               name,"la"st,year
               john,"d"e,1986
               """)
             )

    assert %NimbleCSV.ParseError{
             message: ~s(expected escape character " but reached the end of file),
             line_number: 2
           } ==
             catch_error(
               CSV.parse_string("""
               name,last,year
               john,doe,"1986
               """)
             )
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

    assert %NimbleCSV.ParseError{
             message: ~s'expected escape character " but reached the end of file',
             line_number: 2
           } ==
             catch_error(
               CSV.parse_enumerable([
                 "name,last,year\n",
                 "john,doe,\"1986\n"
               ])
             )
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
          "alex,jones,1980\n",
          "john,doe,\"1986\n"
        ]
        |> Stream.map(&String.upcase/1)
      )

    assert %NimbleCSV.ParseError{
             message: ~s'expected escape character " but reached the end of file',
             line_number: 3
           } == catch_error(Enum.to_list(stream))
  end

  test "dump_to_iodata/1" do
    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["john", 27]])) == """
           name,age
           john,27
           """

    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["john\ndoe", 27]])) == """
           name,age
           "john
           doe",27
           """

    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["john \"nick\" doe", 27]])) ==
             """
             name,age
             "john ""nick"" doe",27
             """

    assert IO.iodata_to_binary(CSV.dump_to_iodata([["name", "age"], ["doe, john", 27]])) == """
           name,age
           "doe, john",27
           """
  end

  test "dump_to_stream/1" do
    assert IO.iodata_to_binary(Enum.to_list(CSV.dump_to_stream([["name", "age"], ["john", 27]]))) ==
             """
             name,age
             john,27
             """

    assert IO.iodata_to_binary(
             Enum.to_list(CSV.dump_to_stream([["name", "age"], ["john\ndoe", 27]]))
           ) == """
           name,age
           "john
           doe",27
           """

    assert IO.iodata_to_binary(
             Enum.to_list(CSV.dump_to_stream([["name", "age"], ["john \"nick\" doe", 27]]))
           ) == """
           name,age
           "john ""nick"" doe",27
           """
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

      assert %NimbleCSV.ParseError{
               message: ~s'expected escape character " but reached the end of file',
               line_number: 2
             } == catch_error(Enum.to_list(stream))
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

  # TODO: Remove once we depend on Elixir 1.3 and on.
  Code.ensure_loaded(String)

  if function_exported?(String, :trim, 1) do
    defp string_trim(str), do: String.trim(str)
  else
    defp string_trim(str), do: String.strip(str)
  end
end
