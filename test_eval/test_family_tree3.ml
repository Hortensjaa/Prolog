open Base
open! Stdio

open Evaluation.Eval

let clauses = Examples.Family_tree3.clauses

let%test_unit "test parent1" =
  [%test_eq: bool] (eval "parent(X, basia)" clauses) true

let%expect_test "test parent1" = 
  let _ = eval "parent(X, basia)" clauses in ();
  [%expect {|
    X: ania |}]

let%test_unit "test parent2" = 
  [%test_eq: bool] (eval "parent(X, kacper)" clauses) true

let%expect_test "test parent2" = 
  let _ = eval "parent(X, kacper)" clauses in ();
  [%expect {|
    X: olek |}]

let%test_unit "test parent3" = 
  [%test_eq: bool] (eval "parent(A, B)" clauses) true

let%expect_test "test parent3" = 
  let _ = eval "parent(A, B)" clauses in ();
  [%expect {|
    A: ania
    B: basia |}]
