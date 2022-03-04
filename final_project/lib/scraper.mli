type t
(**The abstract type of dining locations and their links**)

type m 
(**The abstract type of menus*)

val from_net_nutrition : string -> t
(** [from_net_nutrition s] is the dining locations and links
    from [s] (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)

val get_location : t -> string -> string
(** [from_location t s] is the link to location [string]
    in the main net nutrition page [t].**)

val from_location : string -> m
(** [from_location s] is the menu of dining location with link [s]**)