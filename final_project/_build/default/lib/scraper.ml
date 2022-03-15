open Soup

exception URL_Error of string

type d = {
  name : string;
  location : string;
  contact : string;
  ophours : string list;
  description : string;
}

type m = {
  eatery : d;
  menu_name : string;
  hours : int list;
  menu_items : (string * string list) list;
}

(* This function was copied from:
   https://stackoverflow.com/questions/4621454/reading-html-contents-of-a-url-in-ocaml*)

let string_of_uri uri =
  try
    let connection = Curl.init () and write_buff = Buffer.create 1763 in
    Curl.set_writefunction connection (fun x ->
        Buffer.add_string write_buff x;
        String.length x);
    Curl.set_url connection uri;
    Curl.perform connection;
    Curl.global_cleanup ();
    Buffer.contents write_buff
  with _ -> raise (URL_Error uri)

let rec list_after_element lst element inc =
  match lst with
  | [] -> []
  | h :: t ->
      if h = element && inc = false then t
      else if h = element && inc = true then h :: t
      else list_after_element t element inc

let rec list_before_element lst element inc =
  list_after_element (List.rev lst) element inc |> List.rev

let list_between_elements lst beginning ending binc einc =
  list_before_element
    (list_after_element lst beginning binc)
    ending einc

let webpage =
  list_between_elements
    (string_of_uri
       "https://scl.cornell.edu/residential-life/dining/eateries-menus"
    |> parse |> trimmed_texts)
    "indicates healthy choices" "Quick Links" false false
  |> List.filter (fun x -> if x = "Order Online" then false else true)

let active_eateries =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "a[hreflang]" |> to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)

let available_menu_types =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "summary" |> to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)
  |> List.sort_uniq compare

let available_stations =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "strong" |> to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)
  |> List.sort_uniq compare

let rec separate_into_dining_halls (web : 'a list) (active : 'a list) =
  match active with
  | [] -> [ [] ]
  | [ t ] -> [ list_after_element web t true ]
  | h :: t ->
      list_between_elements web h (List.hd t) true false
      :: separate_into_dining_halls
           (list_after_element web (List.hd t) true)
           t

let into_d lst =
  {
    name = List.hd lst;
    location = List.nth lst 1;
    contact = List.nth lst 2;
    ophours =
      list_between_elements lst "Hours" "Description" false false;
    description = List.hd (list_after_element lst "Description" false);
  }

let militarytime time =
  Scanf.sscanf time "%d:%d%c%c" (fun h m t1 t2 ->
      if t1 = 'a' && h = 12 then 0 + m
      else if t1 = 'a' then (100 * h) + m
      else if t1 = 'p' && h = 12 then 1200 + m
      else (100 * h) + m + 1200)

let hour_parse str =
  List.map
    (fun x -> String.trim x |> militarytime)
    (String.split_on_char '-' str)

let parse_station_offering str : string list =
  List.map String.trim (String.split_on_char '*' str)

let rec get_stations (menu : string list) =
  match menu with
  | [] -> [ ("", [ "" ]) ]
  | [ h ] -> [ ("", parse_station_offering h) ]
  | [ h; t ] -> [ (h, parse_station_offering t) ]
  | h :: t ->
      (h, parse_station_offering (List.hd t))
      :: get_stations (List.tl t)

let rec get_menus_helper
    (hall : d)
    (hours : string list)
    (hall_menus : string list)
    (menu_types : string list) : m list =
  match hall_menus with
  | [] -> []
  | h :: t ->
      if List.mem h menu_types = true then
        {
          eatery = hall;
          menu_name = h;
          hours = hour_parse (List.hd hours);
          menu_items =
            get_stations
              (try
                 list_before_element t
                   (List.find (fun x -> List.mem x menu_types) t)
                   false
               with Not_found -> t);
        }
        :: get_menus_helper hall (List.tl hours) t menu_types
      else get_menus_helper hall hours t menu_types

let get_menus hallinfo =
  get_menus_helper (into_d hallinfo) (into_d hallinfo).ophours
    (list_after_element hallinfo "Featuring/Menu" false)
    available_menu_types

(** [from_net_nutrition s] is the dining locations and links from [s]
    (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)
let from_net_nutrition (_s : string) =
  raise (Failure "from_net_nutrition unimplemented")
