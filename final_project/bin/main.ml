open Soup
let _ = (read_file "Lambda Soup - Functional HTML Scraping for OCaml.html" |> parse $ "title" |> R.leaf_text |> print_endline)



