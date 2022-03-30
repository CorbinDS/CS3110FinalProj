open Utils
open Database
open Bogue
module W = Widget
module L = Layout

let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let box_style =
  let open Style in
  let border = mk_border ~radius:5 thick_grey_line in
  create ~border ()

let menu_placeholder = L.empty ~w:600 ~h:0 ()
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

let menu_filter = W.button "Filter"

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
                [ W.label "                           "; menu_filter ];
            ];
          L.flat_of_w [ menu_filter_box ];
        ];
    ]

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
              L.flat_of_w [ menu_display ];
            ];
          L.flat_of_w [ menu_display_box ];
        ];
    ]

let all_dining_inputs () : dining_hall_attributes list =
  [
    (if W.get_text dining_name_input = "" then Nothing
    else Name (W.get_text dining_name_input));
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
        ( int_of_string time_store.open_h,
          int_of_string time_store.close_h ));
  ]

let all_menu_inputs () : menu_attributes list =
  [
    (if W.get_text menu_name_input = "" then Nothing
    else Name (W.get_text menu_name_input));
    (if W.get_text menu_item_input = "" then Nothing
    else Item (W.get_text menu_item_input));
    (if time_store.open_h = "  " && time_store.close_h = "  " then
     Nothing
    else
      Open_During
        ( int_of_string time_store.open_h,
          int_of_string time_store.close_h ));
  ]

let action ti l _ =
  if W.get_state menu_filter then
    W.set_text l
      (try
         pretty_print_menu
           (List.hd (filter_menus (all_menu_inputs ()) menus))
       with Failure x -> "Nothing.")
  else W.set_text l ""

let c = W.connect menu_filter menu_display action Trigger.buttons_down

let layout =
  L.tower
    [
      menu_placeholder;
      L.flat [ menu_filter_layout; menu_display_layout ];
    ]

let board = Bogue.make [ c ] [ layout ]
let main () = Bogue.run board