open Final_project
open Draw
open Database

let _ =
  print_endline
    (pretty_print_menu
       (Database.load_menu "Lunch" "Jansen's Dining Room at Bethe House"))
