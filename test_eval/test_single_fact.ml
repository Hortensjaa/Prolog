open Base
open! Stdio

open Evaluation.Eval

let clauses = "parent(ania, kasia)."

let%test_unit "test1" =
  [%test_eq: bool] (eval "parent(X, Y)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test1" = 
  let _ = eval "parent(X, Y)" clauses  ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: ania
    Y: kasia
    true |}]

let%test_unit "test2" =
  [%test_eq: bool] (eval "parent(ania, Y)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test2" = 
  let _ = eval "parent(ania, Y)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    Y: kasia
    true |}]

let%test_unit "test3" =
  [%test_eq: bool] (eval "parent(X, kasia)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test3" = 
  let _ = eval "parent(X, kasia)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: ania
    true |}]

let%test_unit "test4" =
  [%test_eq: bool] (eval "parent(ania, kasia)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test4" = 
  let _ = eval "parent(ania, kasia)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    true |}]

let%test_unit "test5" =
  [%test_eq: bool] (eval "parent(kasia, ania)" clauses  ~read_line:(fun () -> ".")) false
  
let%expect_test "test5" = 
  let _ = eval "parent(kasia, ania)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {| |}]

let%test_unit "test6" =
  [%test_eq: bool] (eval "sibling(ania, kasia)" clauses  ~read_line:(fun () -> ".")) false
  
let%expect_test "test6" = 
  let _ = eval "sibling(ania, kasia)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {| |}]