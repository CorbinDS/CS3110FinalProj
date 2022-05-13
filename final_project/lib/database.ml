open Soup
open Curl
open Yojson.Basic.Util
open Lymp
open Sys
open Utils

exception URL_Error of string

type d = {
  name : string;
  location : string;
  contact : string;
  ophours : int list list;
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

type ing =
  | Simple of string
  | Complicated of string * ing list

type i = {
  name : string;
  ingredients : ing;
}

let in_time_range hour1 hour2 time =
  if hour1 > hour2 then hour1 <= time && time <= hour2 + 2400
  else hour1 <= time && time <= hour2

let pretty_print_dining (dining : d) =
  "Name: " ^ dining.name ^ "\n" ^ "Location: " ^ dining.location ^ "\n"
  ^ "Contact: " ^ dining.contact ^ "\n" ^ "Operating Hours: "
  ^ String.concat ", "
      (List.map
         (fun s -> String.concat "-" (List.map string_of_int s))
         dining.ophours)
  ^ "\n" ^ "Description: " ^ dining.description

let pretty_print_menu (menu : m) =
  pretty_print_dining menu.eatery
  ^ "\n\n" ^ "Menu Name: " ^ menu.menu_name ^ "\n" ^ "Hours: "
  ^ String.concat "-" (List.map string_of_int menu.hours)
  ^ "\n" ^ "Menu Items: \n"
  ^ String.concat "\n"
      (List.map
         (fun (station, items) ->
           if station = "" then String.concat ", " items
           else station ^ ": " ^ String.concat ", " items)
         menu.menu_items)

let add_d_to_file record file =
  Sys.chdir "database";
  Sys.chdir "dining_halls";
  close_out (open_out (file ^ ".json"));
  yojson_of_d record |> Yojson.Safe.to_file (file ^ ".json");
  Sys.chdir "..";
  Sys.chdir ".."

let add_m_to_file record file =
  Sys.chdir "database";
  Sys.chdir "menus";
  close_out (open_out (file ^ ".json"));
  yojson_of_m record |> Yojson.Safe.to_file (file ^ ".json");
  Sys.chdir "..";
  Sys.chdir ".."

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

let webpage () =
  list_between_elements
    (string_of_uri
       "https://scl.cornell.edu/residential-life/dining/eateries-menus"
    |> parse |> trimmed_texts)
    "indicates healthy choices" "Quick Links" false false
  |> List.filter (fun x -> if x = "Order Online" then false else true)

(** [active eateries] is a list of active eateries for the current day.
    Requires: All of the headings for eateries being displayed in
    https://scl.cornell.edu/residential-life/dining/eateries-menus need
    to be the only elements that are a[hreflang] elements. *)
let active_eateries () =
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
let available_menu_types () =
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
let available_stations () =
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

let dininginfo () =
  separate_into_dining_halls (webpage ()) (active_eateries ())

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

let web_into_d hallinfo =
  {
    name = List.hd hallinfo;
    location = List.nth hallinfo 1;
    contact = List.nth hallinfo 2;
    ophours =
      List.map parse_time_range
        (list_between_elements hallinfo "Hours" "Description" false
           false);
    description =
      List.hd (list_after_element hallinfo "Description" false);
  }

let web_into_m
    (hallinfo : string list)
    (hours : int list list)
    (hall_menu : string list) : m list =
  [
    {
      eatery = web_into_d hallinfo;
      menu_name = "Menu";
      hours = (try List.hd hours with Failure x -> []);
      menu_items = get_menu_items hall_menu;
    };
  ]

let rec web_into_m_list_helper
    (hallinfo : string list)
    (hours : int list list)
    (hall_menus : string list)
    (menu_types : string list) : m list =
  match hall_menus with
  | [] -> []
  | h :: t ->
      if List.mem h menu_types = true then
        {
          eatery = web_into_d hallinfo;
          menu_name = h;
          hours = List.hd hours;
          menu_items =
            get_menu_items
              (try
                 list_before_element t
                   (List.find (fun x -> List.mem x menu_types) t)
                   false
               with Not_found -> t);
        }
        :: web_into_m_list_helper hallinfo (List.tl hours) t menu_types
      else web_into_m_list_helper hallinfo hours t menu_types

let web_into_m_list hallinfo =
  if
    List.length (list_after_element hallinfo "Featuring/Menu" false) = 1
  then
    web_into_m hallinfo (web_into_d hallinfo).ophours
      (list_after_element hallinfo "Featuring/Menu" false)
  else
    web_into_m_list_helper hallinfo (web_into_d hallinfo).ophours
      (list_after_element hallinfo "Featuring/Menu" false)
      (available_menu_types ())

let update_nutritional_information () =
  print_endline "Starting scraping. Wait for end print." |> fun () ->
  get
    (get_module (init ~exec:"python3" ".") "scrape_net_nutrition")
    "update_net_nutrition" []
  |> fun x -> print_endline "Finished scraping."

let update_dining_halls () =
  Sys.chdir "database";
  remove_contents "dining_halls";
  Sys.chdir "..";
  List.map web_into_d (dininginfo ())
  |> List.map (fun x ->
         add_d_to_file x
           (String.map (fun c -> if c = ' ' then '_' else c) x.name))

let update_menus () =
  Sys.chdir "database";
  remove_contents "menus";
  Sys.chdir "..";
  List.map web_into_m_list (dininginfo ())
  |> List.map (fun xs ->
         List.map
           (fun x ->
             add_m_to_file x
               (String.map
                  (fun c -> if c = ' ' then '_' else c)
                  (x.menu_name ^ "_" ^ x.eatery.name)))
           xs)

let menu_items_from_json station =
  to_string (List.hd station)
  :: List.map to_string (to_list (List.nth station 1))
  |> fun list -> (List.hd list, List.tl list)

let dining_hall_from_json json =
  {
    name = json |> member "name" |> to_string;
    location = json |> member "location" |> to_string;
    contact = json |> member "contact" |> to_string;
    ophours =
      json |> member "ophours" |> to_list
      |> List.map (fun xs -> List.map (fun x -> to_int x) (to_list xs));
    description = json |> member "description" |> to_string;
  }

let menu_from_json json =
  {
    eatery = json |> member "eatery" |> dining_hall_from_json;
    menu_name = json |> member "menu_name" |> to_string;
    hours =
      json |> member "hours" |> to_list |> List.map (fun x -> to_int x);
    menu_items =
      json |> member "menu_items" |> to_list
      |> List.map (fun x -> to_list x)
      |> List.map (fun xs -> menu_items_from_json xs);
  }

type dining_hall_attributes =
  | Nothing
  | Dining_Name of string
  | Campus_Location of string
  | Contact of string
  | Open_During of int * int
  | Description of string

type menu_attributes =
  | Nothing
  | Eateries of d list
  | Menu_Name of string
  | Open_During of int * int
  | Item of string

let rec filter_dining_halls
    (attr : dining_hall_attributes list)
    (ds : d list) : d list =
  match attr with
  | [] -> ds
  | Nothing :: t -> filter_dining_halls t ds
  | Dining_Name n :: t ->
      filter_dining_halls t
        (List.filter
           (fun (dining_hall : d) ->
             contains
               (String.lowercase_ascii dining_hall.name)
               (String.lowercase_ascii n))
           ds)
  | Campus_Location l :: t ->
      filter_dining_halls t
        (List.filter
           (fun (dining_hall : d) ->
             contains
               (String.lowercase_ascii dining_hall.location)
               (String.lowercase_ascii l))
           ds)
  | Contact c :: t ->
      filter_dining_halls t
        (List.filter
           (fun (dining_hall : d) ->
             if dining_hall.contact = c then true else false)
           ds)
  | Open_During (o, e) :: t ->
      filter_dining_halls t
        (List.filter
           (fun (dining_hall : d) ->
             List.exists
               (fun hours ->
                 try
                   in_time_range (List.nth hours 0) (List.nth hours 1) o
                   || in_time_range (List.nth hours 0)
                        (List.nth hours 1) e
                 with Failure x -> false)
               dining_hall.ophours)
           ds)
  | Description des :: t ->
      filter_dining_halls t
        (List.filter
           (fun (dining_hall : d) ->
             contains
               (String.lowercase_ascii dining_hall.description)
               (String.lowercase_ascii des))
           ds)

let rec filter_menus (attr : menu_attributes list) (ms : m list) :
    m list =
  match attr with
  | [] -> ms
  | Nothing :: t -> filter_menus t ms
  | Eateries halls :: t ->
      filter_menus t
        (List.filter
           (fun menu ->
             List.exists (fun hall -> hall = menu.eatery) halls)
           ms)
  | Menu_Name name :: t ->
      filter_menus t
        (List.filter
           (fun menu ->
             if
               String.lowercase_ascii menu.menu_name
               = String.lowercase_ascii name
             then true
             else false)
           ms)
  | Open_During (o, e) :: t ->
      filter_menus t
        (List.filter
           (fun (menu : m) ->
             try
               in_time_range (List.nth menu.hours 0)
                 (List.nth menu.hours 1) o
               || in_time_range (List.nth menu.hours 0)
                    (List.nth menu.hours 1) e
             with Failure x -> false)
           ms)
  | Item i :: t ->
      filter_menus t
        (List.filter
           (fun me ->
             List.exists
               (fun (station, items) ->
                 List.exists
                   (fun it ->
                     contains
                       (String.lowercase_ascii it)
                       (String.lowercase_ascii i))
                   items)
               me.menu_items
             && List.length me.menu_items >= 1)
           ms)

let dining_halls =
  Sys.chdir "database";
  Sys.readdir "dining_halls" |> Array.to_list |> fun files ->
  (Sys.chdir "dining_halls";
   List.map (fun file ->
       Yojson.Basic.from_file file |> dining_hall_from_json))
    files
  |> fun d ->
  Sys.chdir "..";
  Sys.chdir "..";
  d

let menus =
  Sys.chdir "database";
  Sys.readdir "menus" |> Array.to_list |> fun files ->
  (Sys.chdir "menus";
   List.map (fun file -> Yojson.Basic.from_file file |> menu_from_json))
    files
  |> fun m ->
  Sys.chdir "..";
  Sys.chdir "..";
  m

let menu_identifier (menu : m) =
  menu.menu_name ^ ": " ^ menu.eatery.name

let get_menu_from_identifier idt =
  List.hd
    (filter_menus
       [
         Menu_Name (List.hd (String.split_on_char ':' idt));
         Eateries
           (filter_dining_halls
              [
                Dining_Name
                  (String.trim
                     (List.nth (String.split_on_char ':' idt) 1));
              ]
              dining_halls);
       ]
       menus)
