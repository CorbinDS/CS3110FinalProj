open Final_project
open Draw
open Database

let _ =
  filter_menus (Item "sausage") menus
  |> List.map (fun m -> print_endline (pretty_print_menu m))
