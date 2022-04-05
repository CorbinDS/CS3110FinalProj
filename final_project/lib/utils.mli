val contains : string -> string -> bool
val list_after_element : 'a list -> 'a -> bool -> 'a list
val list_before_element : 'a list -> 'a -> bool -> 'a list

val list_between_elements :
  'a list -> 'a -> 'a -> bool -> bool -> 'a list

val remove_contents : string -> unit
val times : string array
val parse_time : string -> int
val index_of : 'a -> 'a list -> int
val return_next_element : 'a -> 'a list -> 'a
val return_prev_element : 'a -> 'a list -> 'a
