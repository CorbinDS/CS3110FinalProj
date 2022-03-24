open Final_project
open Draw
open Scraper

let _ = Scraper.from_net_nutrition |> fun () -> window ()
