open Utils
open Bogue
module W = Widget
module L = Layout

let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let box_style =
  let open Style in
  let border = mk_border ~radius:5 thick_grey_line in
  create ~border ()

let menu_placeholder = L.empty ~h:0 ~w:800 ()
let dining_box = W.box ~style:box_style ~h:250 ~w:225 ()
let dining_name_input = W.text_input ~prompt:"Name" ()
let dining_location_input = W.text_input ~prompt:"Location" ()
let dining_contact_input = W.text_input ~prompt:"Contact" ()
let dining_description_input = W.text_input ~prompt:"Description" ()

let dining_open_hour_input =
  Select.create (Array.of_list [ "0:00"; "1:00" ]) 0

let dining_closed_hour_input =
  Select.create (Array.of_list [ "0:00"; "1:00" ]) 0

let dining_layout =
  L.tower
    [
      menu_placeholder;
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

let menu_filter_box = W.box ~style:box_style ~h:500 ~w:275 ()
let menu_name_input = W.text_input ~prompt:"Name" ()
let menu_item_input = W.text_input ~prompt:"Item" ()
let menu_open_hour_input = Select.create times 0
let menu_closed_hour_input = Select.create times 0

let menu_filter_layout =
  L.tower
    [
      menu_placeholder;
      L.superpose
        [
          L.tower
            [
              L.flat_of_w [ W.label "Menu: " ];
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
            ];
          L.flat_of_w [ menu_filter_box ];
        ];
    ]

let layout =
  L.tower ~margins:0
    ~background:(L.color_bg Draw.(lighter (opaque pale_grey)))
    [ menu_filter_layout ]

let board = Bogue.make [] [ layout ]
let main () = Bogue.run board