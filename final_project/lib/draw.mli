type c
(**Type that represents a graphical component**)

val make_component : int -> int -> int -> int -> c list -> c list -> c
(**A method that takes in two ints and makes a component at that
   location, and takes in two component lists to represent parent and
   child components**)

val window : unit -> unit(**Makes a temporary display window that closes when you press [q]**)
