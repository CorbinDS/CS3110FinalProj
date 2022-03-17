open Soup
open Curl
open Yojson.Basic.Util

exception URL_Error of string

type d = {
  name : string;
  location : string;
  contact : string;
  ophours : string list;
  description : string;
}
[@@deriving yojson_of]

type m = {
  eatery : d;
  menu_name : string;
  hours : int list;
  menu_items : (string * string list) list;
}
[@@deriving yojson_of]

let add_d_to_file record =
  yojson_of_d record |> Yojson.Safe.to_file "dining.json"

let add_m_to_file record =
  yojson_of_m record |> Yojson.Safe.to_file "menu.json"

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

(** [active eateries] is a list of active eateries for the current day.
    Requires: All of the headings for eateries being displayed in
    https://scl.cornell.edu/residential-life/dining/eateries-menus need
    to be the only elements that are a[hreflang] elements. =*)
let active_eateries =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "a[hreflang]" |> Soup.to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)

(** [available_menu_types] is a list of all the types of menus available
    for the current day. For example,
    \["Breakfast";"Brunch";"Lunch";"Dinner"\].

    Requires: Only the available menu types should be stored in
    <summary> elements on the webpage
    https://scl.cornell.edu/residential-life/dining/eateries-menus *)
let available_menu_types =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "summary" |> Soup.to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)
  |> List.sort_uniq compare

(** [available_stations] is a list of all the stations available at each
    of the dining locations for the current day. For example,
    ["Allergy Meal Option"; "Beverage Bar"; "Beverage Station"; "Beverages";
 "Breakfast Menu Options"; "Breakfast Station"; "Cereal/Bagel Bar";
 "Dessert Station"; "Desserts"; "Flat Top Grill"; "Fruit & Yogurt Bar";
 "Global"; "Grill Station"; "Halal"; "Hot Traditional Station - Entrees";
 "Hot Traditional Station - Sides"; "House Made Pasta"; "Iron Grill";
 "Island Bar"; "Kosher Station"; "Mexican Station"; "Pasta and Pizza Station";
 "Pizza"; "Pizza Station"; "Salad Bar Station"; "Soup Station"; "Special";
 "Specialty Station"; "Vegan/Vegetarian Offerings"; "Wok"; "Wok/Asian Station"]*)
let available_stations =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "strong" |> Soup.to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)
  |> List.sort_uniq compare

let rec separate_into_dining_halls
    (web : string list)
    (active_halls : string list) =
  match active_halls with
  | [] -> [ [] ]
  | [ t ] -> [ list_after_element web t true ]
  | h :: t ->
      list_between_elements web h (List.hd t) true false
      :: separate_into_dining_halls
           (list_after_element web (List.hd t) true)
           t

let dininginfo = separate_into_dining_halls webpage active_eateries

(** [parse_time_range str] parses a string, in the format of the
    following examples, to an int. Examples: "12:30am - 1:00am" -> \[30;
    100], "12:35pm - 11:58pm" -> \[1235; 2358] Requires: [str] to be in
    the following format: "%d:%d%c%c - %d:%d%c%c" with %d before the
    colon in the range 1-12, %d after the colon in the range 0-59, %c%c
    being am or pm.*)
let parse_time_range str =
  List.map
    (fun x ->
      Scanf.sscanf (String.trim x) "%d:%d%c%c" (fun h m t1 t2 ->
          if t1 = 'a' && h = 12 then 0 + m
          else if t1 = 'a' then (100 * h) + m
          else if t1 = 'p' && h = 12 then 1200 + m
          else (100 * h) + m + 1200))
    (String.split_on_char '-' str)

let parse_station_offering str : string list =
  List.map String.trim (String.split_on_char '*' str)

let rec get_menu_items (menu : string list) =
  match menu with
  | [] -> [ ("", [ "" ]) ]
  | [ h ] -> [ ("", parse_station_offering h) ]
  | [ h; t ] -> [ (h, parse_station_offering t) ]
  | h :: t ->
      (h, parse_station_offering (List.hd t))
      :: get_menu_items (List.tl t)

let into_d hallinfo =
  {
    name = List.hd hallinfo;
    location = List.nth hallinfo 1;
    contact = List.nth hallinfo 2;
    ophours =
      list_between_elements hallinfo "Hours" "Description" false false;
    description =
      List.hd (list_after_element hallinfo "Description" false);
  }

let rec into_m_list_helper
    (hallinfo : string list)
    (hours : string list)
    (hall_menus : string list)
    (menu_types : string list) : m list =
  match hall_menus with
  | [] -> []
  | h :: t ->
      if List.mem h menu_types = true then
        {
          eatery = into_d hallinfo;
          menu_name = h;
          hours = parse_time_range (List.hd hours);
          menu_items =
            get_menu_items
              (try
                 list_before_element t
                   (List.find (fun x -> List.mem x menu_types) t)
                   false
               with Not_found -> t);
        }
        :: into_m_list_helper hallinfo (List.tl hours) t menu_types
      else into_m_list_helper hallinfo hours t menu_types

let into_m_list hallinfo =
  into_m_list_helper hallinfo (into_d hallinfo).ophours
    (list_after_element hallinfo "Featuring/Menu" false)
    available_menu_types

(** [from_net_nutrition] is the ingredients lists for the various items
    on (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)
let from_net_nutrition =
  raise (Failure "from_net_nutrition unimplemented")

let dining_halls = List.map into_d dininginfo
let menus = List.map into_m_list dininginfo
