open Final_project
open Database
open Graphics
open Unix

let auto_update_menus () =
  Sys.chdir "database";
  if Sys.readdir "menus" = [||] then
    Sys.chdir ".." |> update_menus |> fun x ->
    update_dining_halls () |> fun x -> ()
  else
    let modification_time =
      ( Sys.readdir "menus" |> Array.to_list |> List.hd |> fun x ->
        Sys.chdir "menus";
        x |> Unix.stat )
        .st_mtime |> Unix.localtime
    in
    let current_time = Unix.time () |> Unix.localtime in
    let same_day = Utils.is_same_day modification_time current_time in
    if same_day = true then (
      Sys.chdir "..";
      Sys.chdir "..")
    else
      (Sys.chdir "..";
       Sys.chdir "..")
      |> update_menus
      |> fun x ->
      update_dining_halls () |> fun x -> ()

let _ =
  auto_update_menus ();
  Graphics.main ()
