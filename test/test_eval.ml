open Alcotest

open Evaluation.Eval

let test_fact () =
  let query = "parent(X, Y)" in
  let clauses = "parent(ania, kasia)." in
  (check bool) "test_1" true (eval query clauses)

let () =
  run "Eval tests" [
    "test1", [
      test_case "test1" `Quick test_fact;
    ];
  ]