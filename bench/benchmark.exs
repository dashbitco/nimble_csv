sales_csv = "bench/data/10000_sales.csv"
sales_string = File.read!(sales_csv)
sales_list = String.split(sales_string, "\n")
sales_stream = Stream.map(sales_list, & &1)

Benchee.run(
  %{
    "10k lines, 14 columns - NimbleCSV.RFC4180.parse_string/1" => fn -> 
      NimbleCSV.RFC4180.parse_string(sales_string)
    end,

    "10k lines, 14 columns - NimbleCSV.RFC4180.parse_stream/1" => fn -> 
      sales_stream
      |> NimbleCSV.RFC4180.parse_stream()
      |> Enum.to_list()
    end,

    "10k lines, 14 columns - NimbleCSV.RFC4180.parse_enumerable/1" => fn -> 
      NimbleCSV.RFC4180.parse_enumerable(sales_list)
    end
  }
)
