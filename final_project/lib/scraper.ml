open Curl

exception URL_Error of string

type t
(**The abstract type of dining locations and their links**)

type m 
(**The abstract type of menus*)


(*
This function was copied from: 
https://stackoverflow.com/questions/4621454/reading-html-contents-of-a-url-in-ocaml*)

let string_of_uri uri = 
  try let connection = Curl.init () and write_buff = Buffer.create 1763 in
      Curl.set_writefunction connection
              (fun x -> Buffer.add_string write_buff x; String.length x);
      Curl.set_url connection uri;
      Curl.perform connection;
      Curl.global_cleanup ();
      Buffer.contents write_buff;
  with _ -> raise (URL_Error uri)


let from_net_nutrition (s : string) =
  raise (Failure "from_net_nutrition unimplemented")
(** [from_net_nutrition s] is the dining locations and links
    from [s] (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)

let get_location t (s: string) =
  raise (Failure "get_location unimplemented")

  
(** [from_location t s] is the link to location [string]
    in the main net nutrition page [t].**)

let from_location (s : string) = 
  raise (Failure "from_location unimplmented")
(** [from_location s] is the menu of dining location with link [s]**)