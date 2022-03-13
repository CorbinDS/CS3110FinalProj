(* Had to comment out half of this file so that it would compile because
   otherwise it wouldn't build *)

open Soup

exception URL_Error of string
exception ElementNotFound of string

type d = {
  name : string;
  location : string;
  contact : string;
  hours : string list;
  description : string;
}
(**The abstract type of dining locations and their information. **)

type m = {
  eatery : d;
  menu_name : string;
  start_time : int;
  end_time : int;
  menu_items : (string * string list) list;
}
(**The abstract type of the dining hall menus.*)

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

(** [from_net_nutrition s] is the dining locations and links from [s]
    (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)
let from_net_nutrition (_s : string) =
  raise (Failure "from_net_nutrition unimplemented")

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

let get_webpage =
  list_between_elements
    (string_of_uri
       "https://scl.cornell.edu/residential-life/dining/eateries-menus"
    |> parse |> trimmed_texts)
    "indicates healthy choices" "Quick Links" false false
  |> List.filter (fun x -> if x = "Order Online" then false else true)

(* let rec separate_into_dining_halls web active = match web with | []
   -> [ [ "" ] ] | h :: t -> if h = List.hd active then if List.length
   active = 1 then [ list_after_element t (List.hd active) true ] else
   list_between_elements t (List.hd active) (List.hd (List.tl active))
   true :: separate_into_dining_halls t (List.tl active) else
   separate_into_dining_halls t active *)

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
    location = List.nth lst 2;
    contact = List.nth lst 3;
    hours = list_between_elements lst "Hours" "Description" false false;
    description = List.hd (list_after_element lst "Description" false);
  }

let get_active_eateries =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "a[hreflang]" |> to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)

let get_available_menu_types =
  string_of_uri
    "https://scl.cornell.edu/residential-life/dining/eateries-menus"
  |> parse $$ "summary" |> to_list
  |> List.map (fun y ->
         match leaf_text y with
         | None -> ""
         | Some b -> b)
  |> List.sort_uniq compare
