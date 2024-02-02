open Base
open! Stdio

open Evaluation.Eval

let clauses = 
  "woman(alicja).
  parent(alicja, bartek).
  mother(X, Y) :- parent(X, Y), woman(X)."

(* ----------------------------- parent ----------------------------- *)
let%test_unit "test parent fact1" =
  [%test_eq: bool] (eval "parent(X, Y)" clauses ~read_line:(fun () -> ".")) true
  
let%expect_test "test parent fact1" = 
  let _ = eval "parent(X, Y)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: alicja
    Y: bartek
    true |}]

let%test_unit "test parent fact2" =
  [%test_eq: bool] (eval "parent(alicja, Y)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test parent fact2" = 
  let _ = eval "parent(alicja, Y)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    Y: bartek
    true |}]
    
let%test_unit "test parent fact3" =
  [%test_eq: bool] (eval "parent(X, bartek)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test parent fact3" = 
  let _ = eval "parent(X, bartek)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: alicja
    true |}]

let%test_unit "test parent fact4" =
  [%test_eq: bool] (eval "parent(alicja, bartek)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test parent fact4" = 
  let _ = eval "parent(alicja, bartek)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    true |}]

(* ----------------------------- woman ----------------------------- *)
let%test_unit "test woman fact1" =
  [%test_eq: bool] (eval "woman(alicja)" clauses  ~read_line:(fun () -> ".")) true

let%test_unit "test woman fact2" =
  [%test_eq: bool] (eval "woman(X)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test woman fact2" = 
  let _ = eval "woman(X)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    X: alicja
    true |}]

let%test_unit "test woman fact3" =
  [%test_eq: bool] (eval "woman(bartek)" clauses  ~read_line:(fun () -> ".")) false

    (* ----------------------------- mother ----------------------------- *)
let%test_unit "test mother rule1" =
  [%test_eq: bool] (eval "mother(A, B)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test mother rule1" = 
  let _ = eval "mother(A, B)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    A: alicja
    B: bartek
    true |}]

let%test_unit "test mother rule2" =
  [%test_eq: bool] (eval "mother(alicja, B)" clauses  ~read_line:(fun () -> ".")) true

let%test_unit "test mother rule3" =
  [%test_eq: bool] (eval "mother(A, bartek)" clauses  ~read_line:(fun () -> ".")) true

let%test_unit "test mother rule4" =
  [%test_eq: bool] (eval "mother(alicja, bartek)" clauses  ~read_line:(fun () -> ".")) true
  
let%expect_test "test mother rule2" = 
  let _ = eval "mother(alicja, B)" clauses ~read_line:(fun () -> ".") in ();
  [%expect {|
    --- Solution found! Press ';' to find more solutions or '.' to escape
    B: bartek
    true |}]