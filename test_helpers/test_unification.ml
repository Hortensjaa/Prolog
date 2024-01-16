open Alcotest

open Evaluation.Eval
open Parsing.Helpers
open Structure.Ast


let test_1 () =
  let t1 = parse_term "parent(X, Y)" in
  let t2 = parse_term "parent(ania, kasia)" in
  let result = unify t1 t2 in
  let expected = [("X", Atom("ania")); ("Y", Atom("kasia"))] in
  List.iter (fun pair -> (check bool) "test_1" true ((Hashtbl.find result (fst pair)) = (snd pair))) expected;
  (check int) "test_1" 2 (Hashtbl.length result)

let test_2 () =
  let t1 = parse_term "parent(ania, Y)" in
  let t2 = parse_term "parent(ania, kasia)" in
  let result = unify t1 t2 in
  let expected = [("Y", Atom("kasia"))] in
  List.iter (fun pair -> (check bool) "test_2" true ((Hashtbl.find result (fst pair)) = (snd pair))) expected;
  (check int) "test_2" 1 (Hashtbl.length result)

let test_3 () =
  let t1 = parse_term "a(X, Y, constant, Z)" in
  let t2 = parse_term "a(1, yval, constant, zval)" in
  let result = unify t1 t2 in
  let expected = [("X", Num(1)); ("Y", Atom("yval")); ("Z", Atom("zval"))] in
  List.iter (fun pair -> (check bool) "test_3" true ((Hashtbl.find result (fst pair)) = (snd pair))) expected;
  (check int) "test_2" 3 (Hashtbl.length result)

let test_er1 () = 
  let t1 = parse_term "parent(kasia, Y)" in
  let t2 = parse_term "parent(ania, kasia)" in
  let result = (fun () -> let _ = (unify t1 t2) in ()) in 
  check_raises "different atoms" CantUnify result 

let test_er2 () = 
  let t1 = parse_term "parent(X, Y)" in
  let t2 = parse_term "mother(ania, kasia)" in
  let result = (fun () -> let _ = (unify t1 t2) in ()) in 
  check_raises "er1" CantUnify result 

let test_er3 () = 
  let t1 = parse_term "parent(X, Y, Z)" in
  let t2 = parse_term "parent(ania, kasia)" in
  let result = (fun () -> let _ = (unify t1 t2) in ()) in 
  check_raises "er1" CantUnify result 

  let () =
  run "Unification tests" [
    "test1", [
      test_case "test1" `Quick test_1;
    ];
    "test2", [
      test_case "test2" `Quick test_2;
    ];
    "test3", [
      test_case "test3" `Quick test_3;
    ];
    "test_er1", [
      test_case "different atoms" `Quick test_er1;
    ];
    "test_er2", [
      test_case "test_er2" `Quick test_er2;
    ];
    "test_er3", [
      test_case "too many vars" `Quick test_er3;
    ];
  ]