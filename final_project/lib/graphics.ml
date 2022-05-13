open Utils
open Database
open Bogue
module W = Widget
module L = Layout

(* Basic style layouts *)
let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let box_style =
  let open Style in
  let border = mk_border ~radius:5 thick_grey_line in
  create ~border ()

(* Filtering box *)
let menu_placeholder = L.empty ~w:600 ~h:0 ()

(* Dining box *)
let dining_box = W.box ~style:box_style ~h:250 ~w:225 ()
let dining_name_input = W.text_input ~prompt:"Name" ()
let dining_location_input = W.text_input ~prompt:"Location" ()
let dining_contact_input = W.text_input ~prompt:"Contact" ()
let dining_description_input = W.text_input ~prompt:"Description" ()

let dining_layout =
  L.tower
    [
      L.superpose
        [
          L.tower
            [
              L.flat_of_w [ W.label "Dining Hall: " ];
              L.empty ~h:0 ~w:0 ();
              L.flat_of_w [ W.label "Name: "; dining_name_input ];
              L.flat_of_w
                [ W.label "Location: "; dining_location_input ];
              L.flat_of_w [ W.label "Contact: "; dining_contact_input ];
              L.flat_of_w
                [ W.label "Description: "; dining_description_input ];
            ];
          L.flat_of_w [ dining_box ];
        ];
    ]

(* Menu box *)
type open_t = {
  mutable open_h : string;
  mutable close_h : string;
}

let time_store = { open_h = "  "; close_h = "  " }
let store_open index = time_store.open_h <- times.(index)
let store_closed index = time_store.close_h <- times.(index)
let menu_filter_box = W.box ~style:box_style ~h:550 ~w:275 ()
let menu_name_input = W.text_input ~prompt:"Name" ()
let menu_item_input = W.text_input ~prompt:"Item" ()

let menu_open_hour_input =
  Select.create ?action:(Some store_open) times 0

let menu_closed_hour_input =
  Select.create ?action:(Some store_closed) times 0

let possible_menus_button = W.button "Filter"

let menu_filter_layout =
  L.tower
    [
      L.superpose
        [
          L.tower
            [
              L.flat_of_w [ W.label "Filter: " ];
              L.empty ~h:0 ~w:0 ();
              L.flat_of_w [ W.label "   Name: "; menu_name_input ];
              L.flat_of_w [ W.label "   Item: "; menu_item_input ];
              L.flat
                [
                  L.resident (W.label "   Hours: ");
                  menu_open_hour_input;
                  L.resident (W.label " to ");
                  menu_closed_hour_input;
                ];
              dining_layout;
              L.flat_of_w
                [
                  W.label "                           ";
                  possible_menus_button;
                ];
            ];
          L.flat_of_w [ menu_filter_box ];
        ];
    ]

(* Filtered menus box *)
let filtered_display_box = W.box ~style:box_style ~h:550 ~w:300 ()

let filtered_display_label =
  W.label "All menus that match the filters: "

let filtered_menus = W.text_display "" ~h:500 ~w:250
let selected_menu = W.text_display "" ~h:30 ~w:250
let menu_selector_next = W.button "Next"
let menu_selector_back = W.button "Back"
let show_selected_menus = W.button "Show"

let filtered_menus_layout =
  L.tower
    [
      L.superpose
        [
          L.tower
            [
              L.flat_of_w [ filtered_display_label ];
              L.empty ~h:0 ~w:0 ();
              L.make_clip 350 (L.flat_of_w [ filtered_menus ]);
              L.flat_of_w [ W.label "Selected: " ];
              L.flat_of_w [ selected_menu ];
              L.flat_of_w
                [
                  menu_selector_back;
                  menu_selector_next;
                  show_selected_menus;
                ];
            ];
          L.flat_of_w [ W.label "        " ];
          L.flat_of_w [ filtered_display_box ];
        ];
    ]

(* Selected menu display box *)
let menu_display_box = W.box ~style:box_style ~h:550 ~w:300 ()
let menu_display_label = W.label "Menu will be displayed here: "
let menu_display = W.text_display ~h:500 ~w:250 ""

let menu_display_layout =
  L.tower
    [
      L.superpose
        [
          L.tower ~sep:1
            [
              L.flat_of_w [ menu_display_label ];
              L.make_clip 500 (L.flat_of_w [ menu_display ]);
            ];
          L.flat_of_w [ menu_display_box ];
        ];
    ]

(* Filtering dining halls and menus *)
let all_dining_inputs () : dining_hall_attributes list =
  [
    (if W.get_text dining_name_input = "" then Nothing
    else Dining_Name (W.get_text dining_name_input));
    (if W.get_text dining_location_input = "" then Nothing
    else Campus_Location (W.get_text dining_location_input));
    (if W.get_text dining_contact_input = "" then Nothing
    else Contact (W.get_text dining_contact_input));
    (if W.get_text dining_description_input = "" then Nothing
    else Description (W.get_text dining_description_input));
    (if time_store.open_h = "  " && time_store.close_h = "  " then
     Nothing
    else
      Open_During
        (parse_time time_store.open_h, parse_time time_store.close_h));
  ]

let all_menu_inputs () : menu_attributes list =
  [
    (if W.get_text menu_name_input = "" then Nothing
    else Menu_Name (W.get_text menu_name_input));
    (if W.get_text menu_item_input = "" then Nothing
    else Item (W.get_text menu_item_input));
    (if time_store.open_h = "  " && time_store.close_h = "  " then
     Nothing
    else
      Open_During
        (parse_time time_store.open_h, parse_time time_store.close_h));
    Eateries (filter_dining_halls (all_dining_inputs ()) dining_halls);
  ]

(* Actions *)
let possible_menus_action ti l _ =
  if W.get_state possible_menus_button then
    W.set_text l
      (try
         String.concat ""
           (List.map
              (fun m -> Database.menu_identifier m ^ "\n")
              (filter_menus (all_menu_inputs ()) menus))
       with Failure x -> "Nothing.")
  else W.set_text l ""

let change_selected_menu_next ti l _ =
  if W.get_state menu_selector_next then
    W.set_text l
      (return_next_element (W.get_text l)
         (List.map
            (fun m -> Database.menu_identifier m)
            (filter_menus (all_menu_inputs ()) menus)))
  else ()

let change_selected_menu_back ti l _ =
  if W.get_state menu_selector_back then
    W.set_text l
      (return_prev_element (W.get_text l)
         (List.map
            (fun m -> Database.menu_identifier m)
            (filter_menus (all_menu_inputs ()) menus)))
  else ()

let menu_display_action ti l _ =
  if W.get_state show_selected_menus then
    W.set_text l
      (try
         pretty_print_menu
           (get_menu_from_identifier (W.get_text selected_menu))
       with Failure x -> "Nothing.")
  else W.set_text l ""

(* Connections *)
let show_filtered =
  W.connect possible_menus_button filtered_menus possible_menus_action
    Trigger.buttons_down

let select_menu_to_display_next =
  W.connect menu_selector_next selected_menu change_selected_menu_next
    Trigger.buttons_down

let select_menu_to_display_back =
  W.connect menu_selector_back selected_menu change_selected_menu_back
    Trigger.buttons_down

let show_selected =
  W.connect show_selected_menus menu_display menu_display_action
    Trigger.buttons_down

let layout =
  L.tower
    [
      menu_placeholder;
      L.flat
        [
          menu_filter_layout; filtered_menus_layout; menu_display_layout;
        ];
    ]

let board =
  Bogue.make
    [
      show_filtered;
      show_selected;
      select_menu_to_display_next;
      select_menu_to_display_back;
    ]
    [ layout ]

let main () = Bogue.run board