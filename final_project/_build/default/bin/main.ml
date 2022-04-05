open Final_project
open Database
open Graphics

let _ =
  update_dining_halls ();
  update_menus ();
  Graphics.main ()
