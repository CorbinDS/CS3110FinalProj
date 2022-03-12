(* Had to comment out half of this file so that it would compile because
   otherwise it wouldn't build *)

open Soup

exception URL_Error of string
exception ElementNotFound of string

type t
(**The abstract type of dining locations and their information. **)

type m
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

let get_location _t (_s : string) =
  raise (Failure "get_location unimplemented")

(** [from_location s] is the menu of dining location with link [s]**)
let from_location (_s : string) =
  raise (Failure "from_location unimplmented")

let rec list_after_element lst element =
  match lst with
  | [] -> []
  | [ h ] -> if h = element then [] else raise (ElementNotFound element)
  | h :: t -> if h = element then t else list_after_element t element

let rec list_before_element lst element =
  try list_after_element (List.rev lst) element |> List.rev
  with ElementNotFound e -> raise (ElementNotFound e)

let get_trimmed_contents =
  list_before_element
    (list_after_element
       (string_of_uri
          "https://scl.cornell.edu/residential-life/dining/eateries-menus"
       |> parse |> trimmed_texts)
       "104West!")
    "Quick Links"
