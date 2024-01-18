open Base
open! Stdio

open Evaluation.Eval

let clauses = Examples.Complicated.clauses

let%test_unit "test1" =
  [%test_eq: bool] (eval "p(a, g(Y))" clauses) true
