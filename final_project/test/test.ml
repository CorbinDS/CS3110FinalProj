(* Testing Plan: We tested all the functions in Util and two functions
   from Database with OUnit tests. We manually tested every other
   function in Database and Graphics, by having the Cornell eateries
   website and Net Nutrition websites open to see that our GUI was
   displaying all menus, etc according to the information we passed in.
   When we updated the database with the new menus in Cornell eateries,
   we checked both the new database and Cornell eateries to make sure
   they were the same. When loading an abstract data type from the
   database, we checked that the abstract data type had the same
   information as what was in the database. We essentially used the
   system as though we were users. We ommitted testing features that
   were available in the backend, but not implemented on the user end,
   as the user would never come to use it. We believe that this testing
   method demontrates the correctness of the system because it tests the
   user interface in the way that the user would use it, and the unit
   tests make sure the utility functions do what they are supposed to.*)

(* Things to test: Utils: remove_contents (this is one is more
   complicated to test), contains, list_after_element,
   list_before_element, list_between_elements, index_of,
   return_next_element, return_prev_element, parse_time, in_time_range,
   is_same_day. Database: menu_identifier, get_menu_from_identifier*)

open OUnit2
open Final_project
open Utils
open Database

let contains_test
    (name : string)
    (s1 : string)
    (s2 : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (contains s1 s2) ~printer:string_of_bool

let list_after_element_test
    (name : string)
    (lst : int list)
    (element : int)
    (inc : bool)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal expected_output (list_after_element lst element inc)
    ~printer:(fun x -> String.concat ", " (List.map string_of_int x))

let list_before_element_test
    (name : string)
    (lst : int list)
    (element : int)
    (inc : bool)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal expected_output (list_before_element lst element inc)
    ~printer:(fun x -> String.concat ", " (List.map string_of_int x))

let list_between_elements_test
    (name : string)
    (lst : int list)
    (beginning : int)
    (ending : int)
    (binc : bool)
    (einc : bool)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (list_between_elements lst beginning ending binc einc)
    ~printer:(fun x -> String.concat ", " (List.map string_of_int x))

let index_of_test
    (name : string)
    (e : int)
    (l : int list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (index_of e l) ~printer:string_of_int

let return_next_element_test
    (name : string)
    (e : int)
    (lst : int list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (return_next_element e lst)
    ~printer:string_of_int

let return_prev_element_test
    (name : string)
    (e : int)
    (lst : int list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (return_prev_element e lst)
    ~printer:string_of_int

let parse_time_test
    (name : string)
    (str : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_time str) ~printer:string_of_int

let in_time_range_test
    (name : string)
    (hour1 : int)
    (hour2 : int)
    (time : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (in_time_range hour1 hour2 time)
    ~printer:string_of_bool

let is_same_day_test
    (name : string)
    (t1 : Unix.tm)
    (t2 : Unix.tm)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (is_same_day t1 t2)
    ~printer:string_of_bool

let menu_identifier_test (name : string) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (menu_identifier (List.hd (Database.menus ())))
    ~printer:(fun x -> x)

let get_menu_from_identifier_test (name : string) (idt : string) : test
    =
  name >:: fun _ ->
  assert_equal idt
    (menu_identifier (get_menu_from_identifier idt))
    ~printer:(fun x -> x)

let util_tests =
  [
    list_after_element_test "list after 2 [1,2,3] with 2 included"
      [ 1; 2; 3 ] 2 true [ 2; 3 ];
    list_after_element_test "list after 2 [1,2,3] with 2 not included"
      [ 1; 2; 3 ] 2 false [ 3 ];
    list_after_element_test "list after 3 [1,2,3] with 3 included"
      [ 1; 2; 3 ] 3 true [ 3 ];
    list_after_element_test "list after 3 [1,2,3] with 3 not included"
      [ 1; 2; 3 ] 3 false [];
    list_before_element_test "list before 2 [1,2,3] with 2 included"
      [ 1; 2; 3 ] 2 true [ 1; 2 ];
    list_before_element_test "list before 2 [1,2,3] with 2 not included"
      [ 1; 2; 3 ] 2 false [ 1 ];
    list_before_element_test "list before 1 [1,2,3] with 1 included"
      [ 1; 2; 3 ] 1 true [ 1 ];
    list_before_element_test "list before 1 [1,2,3] with 1 not included"
      [ 1; 2; 3 ] 1 false [];
    list_between_elements_test
      "list between 2 and 5 [1,2,3,4,5,6] with 2 and 5 included"
      [ 1; 2; 3; 4; 5; 6 ] 2 5 true true [ 2; 3; 4; 5 ];
    list_between_elements_test
      "list between 2 and 5 [1,2,3,4,5,6] with 2 not included and 5 \
       included"
      [ 1; 2; 3; 4; 5; 6 ] 2 5 false true [ 3; 4; 5 ];
    list_between_elements_test
      "list between 2 and 5 [1,2,3,4,5,6] with 2 included and 5 not \
       included"
      [ 1; 2; 3; 4; 5; 6 ] 2 5 true false [ 2; 3; 4 ];
    list_between_elements_test
      "list between 2 and 5 [1,2,3,4,5,6] with 2 not included and 5 \
       not included"
      [ 1; 2; 3; 4; 5; 6 ] 2 5 false false [ 3; 4 ];
    contains_test
      "two strings where the second string is a subset of the first \
       string"
      "apple" "app" true;
    contains_test
      "two strings that are entirely different from one another. One \
       is not a subset of the other"
      "twelve" "quirky" false;
    index_of_test "find the index of 1" 1 [ 1; 2; 3; 4 ] 0;
    index_of_test "desired value not contained in the list" 3
      [ 2; 4; 6 ] (-1);
    return_next_element_test "elements after does not exist" 1
      [ 3; 2; 1 ] 3;
    return_next_element_test "elements after exists" 3 [ 3; 5; 6 ] 5;
    return_prev_element_test
      "no element before the stated element and thus wraps around" 3
      [ 3; 5; 6 ] 6;
    return_prev_element_test
      "there is an element before the indicated element" 2 [ 3; 2; 1 ] 3;
    parse_time_test "am time" "3:00am" 300;
    parse_time_test "special 12:00am time" "12:00am" 0;
    parse_time_test "12pm time" "12:00pm" 1200;
    parse_time_test "non 12 value pm time" "4:30pm" 1630;
    is_same_day_test "the two times are apart of the same day"
      (Unix.time () |> Unix.localtime)
      (Unix.time () |> Unix.localtime)
      true;
    in_time_range_test "in time range" 300 600 400 true;
    in_time_range_test "not in time range" 300 600 700 false;
    in_time_range_test "not in time range across midnight" 300 100 200
      false;
    in_time_range_test "in time range across midnight" 300 100 0 true;
  ]

let database_tests =
  [
    menu_identifier_test "Bus Stop Bagels" "Menu: Bus Stop Bagels";
    get_menu_from_identifier_test "Green Dragon" "Menu: Green Dragon";
    get_menu_from_identifier_test "Rose House Dinner"
      "Dinner: Rose House Dining Room";
  ]

let suite =
  "test suite for final project"
  >::: List.flatten [ util_tests; database_tests ]

let _ = run_test_tt_main suite
