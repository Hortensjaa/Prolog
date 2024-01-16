open Base
open! Stdio

open Evaluation.Eval

let clauses = "parent(ania, kasia)."

let%test_unit "test1" =
  [%test_eq: bool] (eval "parent(X, Y)" clauses) true
  
let%expect_test "test1" = 
  let _ = eval "parent(X, Y)" clauses in ();
  [%expect {|
    X: ania
    Y: kasia |}]

let%test_unit "test2" =
  [%test_eq: bool] (eval "parent(ania, Y)" clauses) true
  
let%expect_test "test2" = 
  let _ = eval "parent(ania, Y)" clauses in ();
  [%expect {|
    Y: kasia |}]

let%test_unit "test3" =
  [%test_eq: bool] (eval "parent(X, kasia)" clauses) true
  
let%expect_test "test3" = 
  let _ = eval "parent(X, kasia)" clauses in ();
  [%expect {|
    X: ania |}]

let%test_unit "test4" =
  [%test_eq: bool] (eval "parent(ania, kasia)" clauses) true
  
let%expect_test "test4" = 
  let _ = eval "parent(ania, kasia)" clauses in ();
  [%expect {| |}]

let%test_unit "test5" =
  [%test_eq: bool] (eval "parent(kasia, ania)" clauses) false
  
let%expect_test "test5" = 
  let _ = eval "parent(kasia, ania)" clauses in ();
  [%expect {| |}]

let%test_unit "test6" =
  [%test_eq: bool] (eval "sibling(ania, kasia)" clauses) false
  
let%expect_test "test6" = 
  let _ = eval "sibling(ania, kasia)" clauses in ();
  [%expect {| |}]