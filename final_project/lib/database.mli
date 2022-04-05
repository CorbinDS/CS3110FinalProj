type d
(**The abstract type of eateries and their general information for the
   current day. **)

type m
(**The abstract type of the dining hall menus.*)

val dining_halls : d list
(** [dining halls] is a list of the dining halls in the database. *)

val menus : m list
(** [menus] is a list of the menus in the database. *)

val update_nutritional_information : unit -> unit
(** [update_nutritional_information ()] updates the nutritional
    information in the database using information from
    (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)

val update_dining_halls : unit -> unit list
(** [update_dining_halls ()] updates the dining hall information in the
    database. *)

val update_menus : unit -> unit list list
(** [update_menus ()] updates the menu information in the database. *)

val pretty_print_dining : d -> string
(** [pretty_print_dining d] creates a string of dining hall [d]. *)

val pretty_print_menu : m -> string
(** [pretty_print_menu m] creates a string of menu [m]. *)

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

val filter_dining_halls :
  dining_hall_attributes list -> d list -> d list

val filter_menus : menu_attributes list -> m list -> m list
val menu_identifier : m -> string
val get_menu_from_identifier : string -> m