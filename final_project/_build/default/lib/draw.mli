(**Type that represents a graphical component**)
type c


(**A method that takes in two ints and makes a component at that location, 
 and takes in two component lists to represent parent and child components**)
val make_component : int -> int -> int -> int -> c list -> c list -> c