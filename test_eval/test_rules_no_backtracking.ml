open Base
open! Stdio

open Evaluation.Eval

let clauses = 
  "woman(alicja).
  parent(alicja, bartek).
  mother(X, Y) :- parent(X, Y), woman(X)."

let%test_unit "test parent fact" =
  [%test_eq: bool] (eval "parent(X, Y)" clauses) true
  
let%expect_test "test parent fact" = 
  let _ = eval "parent(X, Y)" clauses in ();
  [%expect {|
    X: alicja
    Y: bartek |}]

let%test_unit "test woman fact" =
  [%test_eq: bool] (eval "woman(X)" clauses) true
  
let%expect_test "test woman fact" = 
  let _ = eval "woman(X)" clauses in ();
  [%expect {|
    X: alicja |}]

let%test_unit "test mother rule1" =
  [%test_eq: bool] (eval "mother(A, B)" clauses) true
  
let%expect_test "test mother rule1" = 
  let _ = eval "mother(A, B)" clauses in ();
  [%expect {|
    A: alicja
    B: bartek |}]