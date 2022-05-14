open Str

let parse_time str =
  Scanf.sscanf (String.trim str) "%d:%d%c%c" (fun h m t1 t2 ->
      if t1 = 'a' && h = 12 then 0 + m
      else if t1 = 'a' then (100 * h) + m
      else if t1 = 'p' && h = 12 then 1200 + m
      else (100 * h) + m + 1200)

let hours =
  [ "12"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "11" ]

let minutes = [ "00"; "30" ]

let am_times =
  List.flatten
    (List.map
       (fun h -> List.map (fun m -> h ^ ":" ^ m ^ "am") minutes)
       hours)

let current_day_am_times =
  List.filter
    (fun x ->
      if
        String.sub x 0 2 = "12"
        || String.sub x 0 2 = "1:"
        || String.sub x 0 2 = "2:"
        || String.sub x 0 2 = "3:"
        || String.sub x 0 2 = "4:"
      then false
      else true)
    am_times

let next_day_am_times =
  List.filter
    (fun x ->
      if
        String.sub x 0 2 = "12"
        || String.sub x 0 2 = "1:"
        || String.sub x 0 2 = "2:"
      then true
      else false)
    (List.flatten
       (List.map
          (fun h -> List.map (fun m -> h ^ ":" ^ m ^ "am") minutes)
          hours))

let pm_times =
  List.flatten
    (List.map
       (fun h -> List.map (fun m -> h ^ ":" ^ m ^ "pm") minutes)
       hours)

let times =
  Array.of_list
    (("  " :: (current_day_am_times @ pm_times)) @ next_day_am_times)

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let rec list_after_element lst element inc =
  match lst with
  | [] -> []
  | h :: t ->
      if h = element && inc = false then t
      else if h = element && inc = true then h :: t
      else list_after_element t element inc

let rec list_before_element lst element inc =
  list_after_element (List.rev lst) element inc |> List.rev

let list_between_elements lst beginning ending binc einc =
  list_before_element
    (list_after_element lst beginning binc)
    ending einc

let rec remove_contents path =
  match Sys.is_directory path with
  | true ->
      Sys.readdir path
      |> Array.iter (fun name ->
             remove_contents (Filename.concat path name))
  | false -> Sys.remove path

let index_of e l =
  let rec index_rec i = function
    | [] -> -1
    | hd :: tl -> if hd = e then i else index_rec (i + 1) tl
  in
  index_rec 0 l

let return_next_element e lst =
  List.nth lst
    (if index_of e lst + 1 >= List.length lst then 0
    else index_of e lst + 1)

let return_prev_element e lst =
  List.nth lst
    (if index_of e lst - 1 < 0 then List.length lst - 1
    else index_of e lst - 1)

let is_same_day (t1 : Unix.tm) (t2 : Unix.tm) =
  if
    t1.tm_year = t1.tm_year && t1.tm_mon = t2.tm_mon
    && t1.tm_mday = t2.tm_mday
  then true
  else false
