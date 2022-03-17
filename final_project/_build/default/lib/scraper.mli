type d
(**The abstract type of eateries and their general information for the
   current day. **)

type m
(**The abstract type of the dining hall menus.*)

val from_net_nutrition : string list
(** [from_net_nutrition s] is the dining locations and links from [s]
    (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)

val dining_halls : d list
val menus : m list list
