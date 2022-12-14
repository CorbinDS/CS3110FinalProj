(** Representation of menu and dining hall data.

    This module represents the menus and dining halls. It handles
    updating of that data from the eateries website to the json files as
    well as querying the data from those json files. *)

type d
(**The abstract type of eateries and their general information for the
   current day. **)

type m
(**The abstract type of the dining hall menus.*)

val dining_halls : unit -> d list
(** [dining halls] is a list of the dining halls in the database. *)

val menus : unit -> m list
(** [menus] is a list of the menus in the database. *)

val update_nutritional_information : unit -> unit
(** [update_nutritional_information ()] updates the nutritional
    information in the database using information from
    (http://netnutrition.dining.cornell.edu/NetNutrition/1).**)

val update_dining_halls : unit -> unit list
(** [update_dining_halls ()] updates the dining hall information in the
    database. *)

val update_menus : unit -> unit
(** [update_menus ()] updates the menu information in the database. *)

val pretty_print_dining : d -> string
(** [pretty_print_dining d] creates a string of dining hall [d]. *)

val pretty_print_menu : m -> string
(** [pretty_print_menu m] creates a string of menu [m]. *)

(** Data type to specify whether to check if a time range is strictly
    within another time range or partially within a time range.*)
type in_range_spec =
  | StrictlyWithinRange
  | PartiallyWithinRange

(** Data type for dining hall attributes by which to filter dining halls *)
type dining_hall_attributes =
  | Nothing
  | Dining_Name of string
  | Campus_Location of string
  | Contact of string
  | Open_During of int * int * in_range_spec
  | Description of string

(** Data type for menu attributes by which to filter menus *)
type menu_attributes =
  | Nothing
  | Eateries of d list
  | Menu_Name of string
  | Open_During of int * int * in_range_spec
  | Item of string
  | Avoid of string
  | Ingredient of string

val filter_dining_halls :
  dining_hall_attributes list -> d list -> d list
(** [filter_dining_halls attrs dining_halls] returns a list of dining
    halls filtered by the attributes provided. *)

val filter_menus : menu_attributes list -> m list -> m list
(** [filter_menus attrs dining_halls] returns a list of menus filtered
    by the attributes provided. *)

val menu_identifier : m -> string
(** [menu_identifier menu] creates a unique identifier for the menu m,
    using the menu name and the name of the dining hall.*)

val get_menu_from_identifier : string -> m
(** [get_menu_from_identifier idt] returns the menu used to make the
    menu identifier.*)

type n
(** Abstract data type for the net nutrition information **)

val get_net_nutrition : unit -> n
val edit_menu_jsons : n -> unit