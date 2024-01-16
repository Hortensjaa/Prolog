open Base
open! Stdio

open Evaluation.Eval

let clauses = 
  "woman(alicja).
  parent(alicja, bartek).
  mother(X, Y) :- parent(X, Y), woman(X)."

(* ----------------------------- parent ----------------------------- *)
let%test_unit "test parent fact1" =
  [%test_eq: bool] (eval "parent(X, Y)" clauses) true
  
let%expect_test "test parent fact1" = 
  let _ = eval "parent(X, Y)" clauses in ();
  [%expect {|
    X: alicja
    Y: bartek |}]

let%test_unit "test parent fact2" =
  [%test_eq: bool] (eval "parent(alicja, Y)" clauses) true
  
let%expect_test "test parent fact2" = 
  let _ = eval "parent(alicja, Y)" clauses in ();
  [%expect {|
    Y: bartek |}]
    
let%test_unit "test parent fact3" =
  [%test_eq: bool] (eval "parent(X, bartek)" clauses) true
  
let%expect_test "test parent fact3" = 
  let _ = eval "parent(X, bartek)" clauses in ();
  [%expect {|
    X: alicja |}]

let%test_unit "test parent fact4" =
  [%test_eq: bool] (eval "parent(alicja, bartek)" clauses) true
  
let%expect_test "test parent fact4" = 
  let _ = eval "parent(alicja, bartek)" clauses in ();
  [%expect {| |}]

(* ----------------------------- woman ----------------------------- *)
let%test_unit "test woman fact1" =
  [%test_eq: bool] (eval "woman(alicja)" clauses) true

let%test_unit "test woman fact2" =
  [%test_eq: bool] (eval "woman(X)" clauses) true
  
let%expect_test "test woman fact2" = 
  let _ = eval "woman(X)" clauses in ();
  [%expect {| X: alicja |}]

let%test_unit "test woman fact3" =
  [%test_eq: bool] (eval "woman(bartek)" clauses) false

    (* ----------------------------- mother ----------------------------- *)
let%test_unit "test mother rule1" =
  [%test_eq: bool] (eval "mother(A, B)" clauses) true
  
let%expect_test "test mother rule1" = 
  let _ = eval "mother(A, B)" clauses in ();
  [%expect {|
    A: alicja
    B: bartek |}]

let%test_unit "test mother rule2" =
  [%test_eq: bool] (eval "mother(alicja, B)" clauses) true

let%test_unit "test mother rule3" =
  [%test_eq: bool] (eval "mother(A, bartek)" clauses) true

let%test_unit "test mother rule4" =
  [%test_eq: bool] (eval "mother(alicja, bartek)" clauses) true
  
let%expect_test "test mother rule2" = 
  let _ = eval "mother(alicja, B)" clauses in ();
  [%expect {|
    B: bartek |}]