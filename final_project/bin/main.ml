open Final_project
open Draw
open Database

let _ = List.map (fun m -> print_endline (pretty_print_menu m)) menus
