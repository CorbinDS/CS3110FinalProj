open Soup
let _ = (read_file "Lambda Soup - Functional HTML Scraping for OCaml.html" |> parse $ "title" |> R.leaf_text |> print_endline)


(**
let string_of_uri uri = 
    try let connection = Curl.init () and write_buff = Buffer.create 1763 in
        Curl.set_writefunction connection
                (fun x -> Buffer.add_string write_buff x; String.length x);
        Curl.set_url connection uri;
        Curl.perform connection;
        Curl.global_cleanup ();
        Buffer.contents write_buff;
    with _ -> raise (IO_ERROR uri)
**)