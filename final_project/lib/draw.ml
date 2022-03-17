open Graphics
open Int

type component = {
  parent : component list;
  children : component list;
  x : int;
  y : int;
  w : int;
  h : int;
}

type c = component

let make_component x y w h p ch : component = {
  parent = p;
  children = ch;
  x = x;
  y = y;
  w = w;
  h = h
}

let draw_component c = 
  fill_rect c.x c.y c.w c.h 

let sqr x = x * x
let in_sqrt x = float_of_int x |> sqrt |> int_of_float


let rel_pos x y : int = if x > y then -1 else 1

let pos_tup x1 x2 y1 y2 : int * int = (rel_pos x1 x2, rel_pos y1 y2)

let displayed c : bool = 
  if c.x + c.w > 0 && c.x < size_x () && c.y + c.h > 0 && c.y < size_y () then true else false

let int_mult_float (x : int) (p : float) : int = 
  float_of_int x |> ( *. ) p |> int_of_float
(**Precondition: Speed [s] cannot be negative*)
let rec move_component c nx ny s : c = 
  if c.x != nx && c.y != ny then
    let img = get_image c.x c.y c.w c.h in
    set_color background;
    fill_rect c.x c.y c.w c.h;
    set_color foreground; (*need to make a state(?) variable to track the current drawing color*) 
    if (s < abs (c.x - nx) && s < abs (c.y - nx)) then
      let dir = pos_tup c.x nx c.y ny in
      let new_x = c.x + (fst dir * s) in
      let new_y = c.y + (snd dir * s) in
      draw_image img new_x new_y;
      let new_comp = make_component new_x new_y c.w c.h c.parent c.children in move_component new_comp nx ny s
    else if (s < abs (c.x - nx)) then
      let dir = pos_tup c.x nx c.y ny in
      let new_x = c.x + (fst dir * s) in 
      draw_image img new_x ny;
      let new_comp = make_component new_x ny c.w c.h c.parent c.children in move_component new_comp nx ny s
    else 
      let dir = pos_tup c.x nx c.y ny in
      let new_y = c.y + (snd dir * s) in 
      draw_image img nx new_y;
      let new_comp = make_component nx new_y c.w c.h c.parent c.children in move_component new_comp nx ny s 
  else
    c


let rec clear_kqueue () = 
  if key_pressed () then
    let c = read_key () in ();
    clear_kqueue ()

let rec input_loop () : unit =
  clear_kqueue ();
  if read_key () = 'q' then close_graph ()
  else input_loop ()


(*Method that draws the side container to set meal preferences*)
let pref () =
  let width = int_mult_float (size_x ()) 0.3 in
  let indented_x = int_mult_float width 0.1 in
  let height = size_y () in
  set_color (rgb 181 36 68);
  fill_rect 0 0 width height;

  (**Hard coded labels for demonstration**)
  set_color (rgb 255 255 255);
  moveto indented_x (int_mult_float height 0.9);
  draw_string "Food Restrictions";

  moveto indented_x (int_mult_float height 0.6);
  draw_string "No Dairy";
  moveto indented_x (int_mult_float height 0.5);
  draw_string "No Meat";
  moveto indented_x (int_mult_float height 0.4);
  draw_string "No Shellfish";
  moveto indented_x (int_mult_float height 0.3);
  draw_string "No Tree Nuts";;
  

let window () = 
  open_graph " 1920x1080";
  set_window_title "Menu Interface";
  pref ();
  input_loop ();