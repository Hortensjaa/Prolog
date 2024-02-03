open Base
open! Stdio

open Evaluation.Eval

let clauses = Examples.Family_tree2.parsed

let%test_unit "test parent1" =
  [%test_eq: bool] (eval "parent(X, basia)" clauses  ~read_line:(fun () -> ".")) true

let%expect_test "test parent1" = 
  let _ = eval "parent(X, basia)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: ania
    true |}]

let%test_unit "test parent2" = 
  [%test_eq: bool] (eval "parent(X, kacper)" clauses  ~read_line:(fun () -> ".")) true

let%expect_test "test parent2" = 
  let _ = eval "parent(X, kacper)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: olek
    true |}]

let%test_unit "test parent3" = 
  [%test_eq: bool] (eval "parent(A, B)" clauses  ~read_line:(fun () -> ".")) true

let%expect_test "test parent3" = 
  let _ = eval "parent(A, B)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    A: ania
    B: basia
    true |}]
