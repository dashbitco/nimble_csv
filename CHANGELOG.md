# Changelog for NimbleCSV

## v1.3.0 (2025-06-24)

  * Require Elixir 1.15+
  * Add `generated: true` to `newlines_separator` macro
  * Fix warnings on Elixir 1.20+
  * Document OWASP official recommendations for CSV injections

## v1.2.0 (2021-12-17)

  * Add `to_line_stream/1` for converting streams into line-oriented ones
  * Support the `:escape_formula` option to prevent code injection via CSV

## v1.1.0 (2020-09-30)

  * Fix CSV dumper to use \r\n as the line separator according to the spec
  * Automatically include new lines in the list of reserved characters

## v1.0.0 (2020-09-25)

  * First stable release
