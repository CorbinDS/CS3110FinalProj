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

let start = open_graph ""

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