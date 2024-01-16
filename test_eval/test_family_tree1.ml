open Base
open! Stdio

open Evaluation.Eval

let clauses = Examples.Family_tree1.clauses

let%test_unit "test parent1" =
  [%test_eq: bool] (eval "parent(X, basia)" clauses) true

let%expect_test "test parent1" = 
  let _ = eval "parent(X, basia)" clauses in ();
  [%expect {|
    X: ania |}]

let%test_unit "test parent2" =
  [%test_eq: bool] (eval "parent(olek, ania)" clauses) true

let%test_unit "test man" =
  [%test_eq: bool] (eval "man(olek)" clauses) true

let%test_unit "test father" =
  [%test_eq: bool] (eval "father(olek, ania)" clauses) true

let%test_unit "test father false1" =
  [%test_eq: bool] (eval "father(olek, basia)" clauses) false

let%test_unit "test father false2" =
  [%test_eq: bool] (eval "father(ania, basia)" clauses) false
