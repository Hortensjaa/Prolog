open Base
open! Stdio

open Evaluation.Eval

let clauses = Examples.Family_tree1.parsed

let%test_unit "test parent1" =
  [%test_eq: bool] (eval "parent(X, basia)" clauses  ~read_line:(fun () -> ".")) true

let%expect_test "test parent1" = 
  let _ = eval "parent(X, basia)" clauses  ~read_line:(fun () -> ".")  in ();
  [%expect{|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: ania
    true |}]

let%test_unit "test parent2" =
  [%test_eq: bool] (eval "parent(F, ania)" clauses  ~read_line:(fun () -> ".")) true

let%expect_test "test parent2" = 
  let _ = eval "parent(F, ania)" clauses  ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    F: olek
    true |}]

let%test_unit "test man" =
  [%test_eq: bool] (eval "man(olek)" clauses  ~read_line:(fun () -> ".")) true

let%test_unit "test father" =
  [%test_eq: bool] (eval "father(olek, ania)" clauses  ~read_line:(fun () -> ".")) true

let%test_unit "test father false1" =
  [%test_eq: bool] (eval "father(olek, basia)" clauses  ~read_line:(fun () -> ".")) false

let%test_unit "test father false2" =
  [%test_eq: bool] (eval "father(ania, basia)" clauses  ~read_line:(fun () -> ".")) false

let%test_unit "test siblings" =
  [%test_eq: bool] (eval "sibling(ania, kacper)" clauses  ~read_line:(fun () -> ".")) true

let%test_unit "test siblings2" =
  [%test_eq: bool] (eval "sibling(kacper, ania)" clauses  ~read_line:(fun () -> ".")) true

let%expect_test "test siblings3" = 
  let _ = eval "sibling(Z, ania)"  ~read_line:(fun () -> ".") clauses in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    Z: kacper
    true |}]

