open Base
open! Stdio

open Evaluation.Eval

let clauses = Examples.Ancestors.clauses

let%test_unit "test ancestor1" =
  [%test_eq: bool] (eval "ancestor(olek, ania)" clauses) true

let%test_unit "test ancestor2" =
  [%test_eq: bool] (eval "ancestor(olek, basia)" clauses) true

let%test_unit "test ancestor3" =
  [%test_eq: bool] (eval "ancestor(olek, piotr)" clauses) true

let%test_unit "test ancestor4" =
  [%test_eq: bool] (eval "ancestor(ania, piotr)" clauses) true

let%test_unit "test ancestor5" =
  [%test_eq: bool] (eval "ancestor(ania, basia)" clauses) true

let%test_unit "test ancestor6" =
  [%test_eq: bool] (eval "ancestor(kacper, piotr)" clauses) false

