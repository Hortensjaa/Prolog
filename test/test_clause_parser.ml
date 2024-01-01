open Alcotest

open Parsing.Parser
open Structure

let mother_str = "mother(X, Y)" 
let parent_str = "parent(X, Y)"
let man_str = "man(X)"
let mother_term = Comp(Atom("mother"), [VarS("X"); VarS("Y")])
let parent_term = Comp(Atom("parent"), [VarS("X"); VarS("Y")])
let man_term = Comp(Atom("man"), [VarS("X")])

let pos_str = "positive"
let neg_str = "negative"
let zero_str = "0"
let pos_term = Atom("positive")
let neg_term = Atom("negative")
let zero_term = Num(0)

let test_parse_fact () =
  let result = parse_clause (parent_str  ^ ".") in
  let expected = Fact(parent_term, true) in
  (check bool) "parse_fact" true (result=expected)

let test_parse_neg () =
  let result = parse_clause ("\\+ " ^ parent_str  ^ ".") in
  let expected = Fact(parent_term, false) in
  (check bool) "parse_neg" true (result=expected)

let test_parse_rule1 () =
  let result = parse_clause (parent_str ^ " :- " ^ mother_str  ^ ".") in
  let expected = Rule((parent_term, true), (mother_term, true)) in
  (check bool) "parse_rule" true (result=expected)

let test_parse_rule2 () =
  let result = parse_clause ("\\+ " ^ mother_str ^ " :- \\+ " ^ parent_str  ^ ".") in
  let expected = Rule((mother_term, false), (parent_term, false)) in
  (check bool) "parse_rule" true (result=expected)

let test_parse_disj1 () =
  let result = parse_clause ("\\+ " ^ mother_str ^ " :- \\+ " ^ parent_str  ^ "; " ^ man_str ^ ".") in
  let expected = DRule((mother_term, false), [(parent_term, false); (man_term, true)]) in
  (check bool) "parse_disj1" true (result=expected)

let test_parse_disj2 () =
  let result = parse_clause ("\\+ " ^ neg_str ^ " :- " ^ pos_str  ^ "; " ^ zero_str ^ ".") in
  let expected = DRule((neg_term, false), [(pos_term, true); (zero_term, true)]) in
  (check bool) "parse_disj2" true (result=expected)

let test_parse_conj1 () =
  let result = parse_clause (mother_str ^ " :- " ^ parent_str  ^ ", \\+ " ^ man_str ^ ".") in
  let expected = CRule((mother_term, true), [(parent_term, true); (man_term, false)]) in
  (check bool) "parse_conj1" true (result=expected)

let test_parse_conj2 () =
  let result = parse_clause (pos_str ^ " :- \\+ " ^ neg_str  ^ ", \\+ " ^ zero_str ^ ".") in
  let expected = CRule((pos_term, true), [(neg_term, false); (zero_term, false)]) in
  (check bool) "parse_conj2" true (result=expected)

let test_parse_conj3 () =
  let result = parse_clause (pos_str ^ " :- \\+ " ^ neg_str  ^ ", \\+ " ^ zero_str ^ ", true.") in
  let expected = CRule((pos_term, true), [(neg_term, false); (zero_term, false); (Atom("true"), true)]) in
  (check bool) "parse_conj3" true (result=expected)

let () =
  let open Alcotest in
  run "Clauses' parser tests" [
    "test_parse_fact", [
      test_case "test_parse_fact" `Quick test_parse_fact;
    ];
    "test_parse_neg", [
      test_case "test_parse_neg" `Quick test_parse_neg;
    ];
    "test_parse_rule1", [
      test_case "test_parse_rule" `Quick test_parse_rule1;
    ];
    "test_parse_rule2", [
      test_case "test_parse_rule" `Quick test_parse_rule2;
    ];
    "test_parse_disj1", [
      test_case "test_parse_disj" `Quick test_parse_disj1;
    ];
    "test_parse_disj2", [
      test_case "test_parse_disj" `Quick test_parse_disj2;
    ];
    "test_parse_conj1", [
      test_case "test_parse_conj" `Quick test_parse_conj1;
    ];
    "test_parse_conj2", [
      test_case "test_parse_conj" `Quick test_parse_conj2;
    ];
    "test_parse_conj3", [
      test_case "test_parse_conj" `Quick test_parse_conj3;
    ];
  ]
