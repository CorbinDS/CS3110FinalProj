(** Utility functions for the other modules. *)

val remove_contents : string -> unit
(** [remove_contents folder] removes the contents of the folder.
    Requires: folder has to be a folder within the current directory.*)

val contains : string -> string -> bool
(** [contains s1 s2] returns true if s2 is in s1. *)

val list_after_element : 'a list -> 'a -> bool -> 'a list
(** [list_after_element lst element inc] returns the list after the
    first instance of [element] in [lst]. [inc] indicates whether
    [element] should be included or not. Requires: [element] is in [lst]*)

val list_before_element : 'a list -> 'a -> bool -> 'a list
(** [list_before_element lst element inc] returns the list before the
    first instance of [element] in [lst]. [inc] indicates whether
    [element] should be included or not. Requires: [element] is in [lst]*)

val list_between_elements :
  'a list -> 'a -> 'a -> bool -> bool -> 'a list
(** [list_between_elements lst element1 element2 inc1 inc2] returns the
    list between the first instance of [element1] and the first instance
    of [element2] in [lst]. [inc1] indicates whether [element1] should
    be included or not.[inc2] indicates whether [element2] should be
    included or not Requires: [element1] and [element2] are in [lst]*)

val index_of : 'a -> 'a list -> int
(** [index_of element lst] returns the index 0 - (length - 1) of the
    element. If [element] is not in [lst], it returns -1. *)

val return_next_element : 'a -> 'a list -> 'a
(** [return_next_element element lst] returns the element in the list
    after [element]. If at the end of the list, circles back to the
    beginning of the list. *)

val return_prev_element : 'a -> 'a list -> 'a
(** [return_next_element element lst] returns the element in the list
    before [element]. If at the beginning of the list, circles back to
    the end of the list. *)

val today_times : string list
(** [today_times] is a list of times from 5:00am to 11:30pm with the
    same format. *)

val tomorrow_times : string list
(** [tomorrow_times] is a list of times from 12:00am to 2:30am with the
    same format. *)

val times : string array
(** [times] is a list of times from 5:00am to 2:30am, with the same
    format displayed, prepended by " ".*)

val parse_time : string -> int
(** [parse_time str] parses strings in the following format: %d:%d%c%c
    into military time. For instance, 12:00am would become 0. 2:25pm
    would become 1425. Requires: [str] be in the format of hh:mmam or
    hh:mmpm. *)

val in_time_range : int -> int -> int -> bool
(** [in_time_range str_time end_time time] checks if time is in between
    str_time and end_time inclusive. Requires: [str_time], [end_time],
    and [time] be in military time. *)

val is_same_day : Unix.tm -> Unix.tm -> bool
(** [is_same_day date1 date2] checks if date1 and date2 are the same
    day. *)
