open Utils
open Database
open Bogue
module W = Widget
module L = Layout

(* Some background about Bogue. *)
(* Widgets: the building blocks of the GUI that receive all events. *)
(* Layouts: in order for widgets to be displayed, they need to be
   packaged into Layouts. *)
(* A Connection has a source widget and a target widget. When the source
   widget receives a specified event, the connection is activated,
   executing a specified function, which is called an Action. *)

(* ----- Basic style layouts ----- *)
let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let box_style =
  let open Style in
  let border = mk_border ~radius:5 thick_grey_line in
  create ~border ()

(* ----- Filter Input Box Widgets and Layouts ----- *)
let menu_filter_box = W.box ~style:box_style ~h:650 ~w:275 ()
let dining_name_input = W.text_input ~prompt:"Name  " ()
let dining_location_input = W.text_input ~prompt:"Location  " ()
let bad_ingredient_input = W.text_input ~prompt:"Ingredient  " ()
let menu_name_input = W.text_input ~prompt:"Name  " ()
let menu_item_input = W.text_input ~prompt:"Items  " ()
let menu_avoid_input = W.text_input ~prompt:"Items  " ()

type open_t = {
  mutable open_h : string;
  mutable close_h : string;
}

(* For storing open and closing time ranges in the filter input. *)
let time_store = { open_h = " "; close_h = " " }
let store_open index = time_store.open_h <- times.(index)
let store_closed index = time_store.close_h <- times.(index)

(* Select list for choosing open time for the input box *)
let menu_open_hour_input =
  Select.create ?action:(Some store_open) times 0

(* Select list for choosing end time for the input box *)
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
              L.flat_of_w
                [ W.label ~size:18 "Filter Available Dining Halls  |" ];
              L.empty ~h:0 ~w:0 ();
              L.flat_of_w
                [ W.label "   Preferred Items: "; menu_item_input ];
              L.flat_of_w
                [ W.label "   Items to avoid: "; menu_avoid_input ];
              L.flat_of_w
                [ W.label "Dining Hall Name: "; dining_name_input ];
              L.flat_of_w
                [ W.label "Campus Location: "; dining_location_input ];
              L.flat
                [
                  L.resident (W.label "   Hours: ");
                  menu_open_hour_input;
                  L.resident (W.label " to ");
                  menu_closed_hour_input;
                ];
              L.flat_of_w
                [
                  W.label
                    "   Check if menu is available at any point in \
                     this range     |"
                    ~size:10;
                ];
              L.flat_of_w
                [ W.label ~size:18 "Ingredient Filtering     " ];
              L.flat_of_w
                [
                  W.label ~size:10
                    "Will limit meal options to Net Nutrition eateries \
                     only    ";
                ];
              L.flat_of_w
                [
                  W.label "Ingredients to avoid   ";
                  bad_ingredient_input;
                ];
              L.flat_of_w
                [
                  W.label "                         ";
                  possible_menus_button;
                ];
            ];
          L.flat_of_w [ menu_filter_box ];
        ];
    ]
    ~background:(L.color_bg (Draw.opaque Draw.pale_grey))

(* ----- Filtered menus widgets and layouts ----- *)
let filtered_display_box = W.box ~style:box_style ~h:530 ~w:300 ()

let filtered_menus_text_display =
  W.text_display
    (String.concat ""
       (List.map
          (fun m -> Database.menu_identifier m ^ "\n")
          (filter_menus [] (menus ()))))
    ~h:2000 ~w:250

let selected_menu_text =
  W.text_display "Select a menu using the Back, Next, and Show buttons."
    ~h:30 ~w:250

let menu_selector_next = W.button "Next"
let menu_selector_back = W.button "Back"
let show_selected_menus = W.button "Show"

let filtered_menus_layout =
  L.tower
    [
      L.superpose
        [
          L.tower ~sep:0
            [
              L.flat_of_w [ W.label "Filtered Menus: " ];
              L.empty ~h:0 ~w:0 ();
              L.make_clip 350
                (L.flat_of_w [ filtered_menus_text_display ]);
              L.flat_of_w [ W.label "Selected: " ];
              L.flat_of_w [ selected_menu_text ];
              L.flat_of_w
                [
                  menu_selector_back;
                  menu_selector_next;
                  show_selected_menus;
                ];
            ];
          L.flat_of_w [ filtered_display_box ];
        ];
    ]

(* ----- Update menus and dining halls widgets and layouts -----*)
let update_box = W.box ~style:box_style ~h:85 ~w:300 ()
let update_menus_button = W.button "Update Menus"

let update_nutritional_information_button =
  W.button "Update Nutri. Info."

let update_nutritional_information_button_resident =
  L.resident update_nutritional_information_button

let update_box_layout =
  L.tower ~vmargin:0
    [
      L.superpose ~center:true
        [
          L.tower ~sep:1
            [
              L.flat_of_w [ W.label "Update   x" ];
              L.flat
                [
                  L.resident update_menus_button;
                  update_nutritional_information_button_resident;
                ];
            ];
          L.flat_of_w [ update_box ];
        ];
    ]

(* Popup that shows up if you hover over the "Update Nutr. Info."
   button. *)
let nutritional_information_notice =
  Popup.tooltip "Note: this will take about 20 minutes to finish."
    ~position:Popup.Below
    ~target:update_nutritional_information_button_resident
    update_nutritional_information_button update_box_layout

(* ----- Selected menus widgets and layouts ----- *)
let menu_display_box = W.box ~style:box_style ~h:650 ~w:300 ()
let menu_display = W.text_display ~h:2000 ~w:250 ""
let menu_displayed = W.text_display ~h:32 ~w:(225 - 75) ""

(* For storing the open and close times that you want to use when adding
   the menu to the calendar*)
let calendar_input_store = { open_h = " "; close_h = " " }

let calendar_store_open index =
  calendar_input_store.open_h <- times.(index)

let calendar_store_closed index =
  calendar_input_store.close_h <- times.(index)

let calendar_start_menu_input =
  Select.create ?action:(Some calendar_store_open) times 0

let calendar_end_menu_input =
  Select.create ?action:(Some calendar_store_closed) times 0

let add_to_calendar_button = W.button "Add to Calendar"
let clear_calendar_button = W.button "Clear Entire Calendar"

let menu_display_layout =
  L.tower
    [
      L.superpose
        [
          L.tower ~sep:1
            [
              L.flat_of_w [ W.label "Selected Menu: " ];
              L.make_clip 350 (L.flat_of_w [ menu_display ]);
              L.flat_of_w
                [
                  W.text_display ~h:32 ~w:75 "Menu displayed: ";
                  menu_displayed;
                ];
              L.flat_of_w
                [
                  W.text_display ~h:32 ~w:225
                    "Add menu at the following times: ";
                ];
              L.flat
                [
                  L.resident (W.label "Start: ");
                  calendar_start_menu_input;
                  L.resident (W.label "  ");
                  L.resident (W.label "End: ");
                  calendar_end_menu_input;
                ];
              L.flat_of_w [ add_to_calendar_button ];
              L.flat_of_w [ clear_calendar_button ];
            ];
          L.flat_of_w [ menu_display_box ];
        ];
    ]

(* ---------- Calendar display widgets and layouts ----------*)
let calendar_display_box = W.box ~style:box_style ~h:650 ~w:375 ()
let calendar_display_label = W.label "Today's Calendar:       |"

(* Times on the calendar*)
let time_list = List.tl (Array.to_list times)

(* Menus put on the calendar *)
let menu_list =
  ref (List.map (fun x -> "") (List.tl (Array.to_list times)))

(* ----- Start of Calendar Creation -----*)
let day_col =
  let day =
    Array.of_list
      (List.map (fun x -> "Today") today_times
      @ List.map (fun x -> "Tomorrow") tomorrow_times)
  in
  Table.
    {
      title = "Day";
      length = Array.length times - 1;
      rows = (fun i -> L.resident (W.label day.(i)));
      compare = None;
      width = Some 75;
    }

let time_col =
  let time_array = Array.of_list time_list in
  Table.
    {
      title = "Time";
      length = Array.length times - 1;
      rows = (fun i -> L.resident (W.label time_array.(i)));
      compare = None;
      width = Some 55;
    }

let menu_col =
  let menu_array = Array.of_list !menu_list in
  Table.
    {
      title = "Menu";
      length = Array.length times - 1;
      rows = (fun i -> L.resident (W.label menu_array.(i)));
      compare = None;
      width = Some 200;
    }

let calendar_display, _ =
  Table.create ~h:575 [ day_col; time_col; menu_col ]

(* -------- End of Calendar Creation --------*)

let calendar_display_layout =
  L.tower
    [
      L.superpose
        [
          L.tower
            [
              L.flat_of_w [ calendar_display_label ];
              L.flat [ calendar_display ];
            ];
          L.flat_of_w [ calendar_display_box ];
        ];
    ]

(** [all_dining_inputs ()] gets all the inputs from the various widgets
    in the filter input box and returns a list of dining hall
    attributes. *)
let all_dining_inputs () : dining_hall_attributes list =
  [
    (if W.get_text dining_name_input = "" then Nothing
    else Dining_Name (W.get_text dining_name_input));
    (if W.get_text dining_location_input = "" then Nothing
    else Campus_Location (W.get_text dining_location_input));
  ]

(** [all_menu_inputs ()] gets all the inputs from the various widgets in
    the filter input box and returns a list of menu hall attributes. *)
let all_menu_inputs () : menu_attributes list =
  [
    (if W.get_text menu_name_input = "" then Nothing
    else Menu_Name (W.get_text menu_name_input));
    (if W.get_text menu_item_input = "" then Nothing
    else Item (W.get_text menu_item_input));
    (if W.get_text menu_avoid_input = "" then Nothing
    else Avoid (W.get_text menu_avoid_input));
    (if W.get_text bad_ingredient_input = "" then Nothing
    else Ingredient (W.get_text bad_ingredient_input));
    (if time_store.open_h = " " && time_store.close_h = " " then Nothing
    else
      Open_During
        ( parse_time time_store.open_h,
          parse_time time_store.close_h,
          PartiallyWithinRange ));
    Eateries
      (filter_dining_halls (all_dining_inputs ()) (dining_halls ()));
  ]

(* ---------- Actions ----------*)

(** [possible_menus_action ti l _] shows the menus that adhere to the
    inputs given when the filter button is pressed.*)
let possible_menus_action ti l _ =
  if W.get_state possible_menus_button then (
    W.set_text l
      (try
         String.concat ""
           (List.map
              (fun m -> Database.menu_identifier m ^ "\n")
              (filter_menus (all_menu_inputs ()) (menus ())))
       with Failure x -> "Nothing.");
    W.set_text selected_menu_text "")
  else ()

(** [change_selected_menu_next ti l _] shows the name of the next menu
    in the list of menus that match the filter inputs given.*)
let change_selected_menu_next ti l _ =
  if W.get_state menu_selector_next then
    W.set_text l
      (return_next_element (W.get_text l)
         (List.map
            (fun m -> Database.menu_identifier m)
            (filter_menus (all_menu_inputs ()) (menus ()))))
  else ()

(** [change_selected_menu_back ti l _] shows the name of the previous
    menu in the list of menus that match the filter inputs given *)
let change_selected_menu_back ti l _ =
  if W.get_state menu_selector_back then
    W.set_text l
      (return_prev_element (W.get_text l)
         (List.map
            (fun m -> Database.menu_identifier m)
            (filter_menus (all_menu_inputs ()) (menus ()))))
  else ()

(** [menu_display_action ti l _ ] shows menu of the menu selected *)
let menu_display_action ti l _ =
  if W.get_state show_selected_menus then (
    W.set_text l
      (try
         pretty_print_menu
           (get_menu_from_identifier (W.get_text selected_menu_text))
       with Failure x -> "Nothing.");
    W.set_text menu_displayed (W.get_text selected_menu_text))
  else ()

(** [update_menus_action w] updates the menus in the database upon the
    click of the update menus button.*)
let update_menus_action w =
  if W.get_state update_menus_button then
    update_menus () |> fun x ->
    update_dining_halls () |> fun x -> ()
  else ()

(** [update_nutritional_information_button_action w] updates the
    nutritional information in the database upon the click of the update
    nutritional information button.*)
let update_nutritional_information_button_action w =
  if W.get_state update_nutritional_information_button then
    update_nutritional_information () |> fun x ->
    update_dining_halls () |> fun x ->
    update_menus () |> fun x -> ()
  else ()

(** [add_to_calendar_action w] adds the menu whose menu items are being
    shown. There needs to be a menu being shown, with the times being
    selected within the range of availability for the menu. *)
let add_to_calendar_action w =
  if W.get_state add_to_calendar_button then (
    let beg_time = calendar_input_store.open_h in
    let end_time = calendar_input_store.close_h in
    let menu_id_to_add = W.get_text menu_displayed in
    let menu_to_add =
      if beg_time = " " || end_time = " " || menu_id_to_add = "" then []
      else
        filter_menus
          [
            Open_During
              ( parse_time beg_time,
                parse_time end_time,
                StrictlyWithinRange );
          ]
          [ get_menu_from_identifier menu_id_to_add ]
    in
    if List.length menu_to_add < 1 then ()
    else
      menu_list :=
        List.map2
          (fun tm m ->
            if
              in_time_range (parse_time beg_time) (parse_time end_time)
                (parse_time tm)
              = true
            then menu_id_to_add
            else m)
          time_list !menu_list;
    let menu_col =
      let menu_array = Array.of_list !menu_list in
      Table.
        {
          title = "Menu";
          length = Array.length times - 1;
          rows = (fun i -> L.resident (W.label menu_array.(i)));
          compare = None;
          width = Some 160;
        }
    in
    let new_calendar, _ =
      Table.create ~h:575 [ day_col; time_col; menu_col ]
    in
    L.set_rooms ~sync:true calendar_display [ new_calendar ])

(** [clear_calendar_action w] clears the calendar of all the menus put
    in. *)
let clear_calendar_action w =
  menu_list := List.map (fun m -> "") !menu_list;
  let menu_col =
    let menu_array = Array.of_list !menu_list in
    Table.
      {
        title = "Menu";
        length = Array.length times - 1;
        rows = (fun i -> L.resident (W.label menu_array.(i)));
        compare = None;
        width = Some 160;
      }
  in
  let new_calendar, _ =
    Table.create ~h:575 [ day_col; time_col; menu_col ]
  in
  L.set_rooms ~sync:true calendar_display [ new_calendar ]

(* ---------- Connections ----------*)
(* These all the connect the functions to their correspondingly named
   buttons. *)
let show_filtered =
  W.connect possible_menus_button filtered_menus_text_display
    possible_menus_action
    (Trigger.buttons_up @ Trigger.buttons_down)

let select_menu_to_display_next =
  W.connect menu_selector_next selected_menu_text
    change_selected_menu_next
    (Trigger.buttons_up @ Trigger.buttons_down)

let select_menu_to_display_back =
  W.connect menu_selector_back selected_menu_text
    change_selected_menu_back
    (Trigger.buttons_up @ Trigger.buttons_down)

let show_selected =
  W.connect show_selected_menus menu_display menu_display_action
    (Trigger.buttons_up @ Trigger.buttons_down)

let update_menus_connection =
  W.on_release update_menus_action update_menus_button

let update_nutritional_information_connection =
  W.on_release update_nutritional_information_button_action
    update_nutritional_information_button

let add_to_calendar_connection =
  W.on_release add_to_calendar_action add_to_calendar_button;
  W.on_click add_to_calendar_action add_to_calendar_button

let clear_calendar_connection =
  W.on_release clear_calendar_action clear_calendar_button;
  W.on_click clear_calendar_action clear_calendar_button

(* Overall Layout of the GUI *)
let layout =
  L.tower
    [
      L.empty ~w:650 ~h:0 ();
      L.flat
        [
          L.flat
            [
              L.tower [ menu_filter_layout ];
              L.tower ~sep:1
                [ filtered_menus_layout; update_box_layout ];
            ];
          L.tower [ menu_display_layout ];
          L.tower [ calendar_display_layout ];
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