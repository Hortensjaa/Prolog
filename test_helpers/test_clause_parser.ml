open Alcotest

open Parsing.Clause_parser
open Structure.Ast

let mother_str = "mother(X, Y)" 
let parent_str = "parent(X, Y)"
let woman_str = "woman(X)"
let mother_term = Comp(Atom("mother"), [VarS("X"); VarS("Y")])
let parent_term = Comp(Atom("parent"), [VarS("X"); VarS("Y")])
let woman_term = Comp(Atom("woman"), [VarS("X")])

let pos_str = "positive"
let neg_str = "negative"
let zero_str = "0"
let pos_term = Atom("positive")
let neg_term = Atom("negative")
let zero_term = Num(0)

let test_parse_fact () =
  let result = parse_clause (parent_str  ^ ".") in
  let expected = Fact(parent_term) in
  (check bool) "parse_fact" true (result=[expected])

let test_parse_rule1 () =
  let result = parse_clause (parent_str ^ " :- " ^ mother_str  ^ ".") in
  let expected = Rule(parent_term, [mother_term]) in
  (check bool) "parse_rule" true (result=[expected])

let test_parse_disj2 () =
  let result = parse_clause (neg_str ^ " :- " ^ pos_str  ^ "; " ^ zero_str ^ ".") in
  let expected = [
    Rule(neg_term, [pos_term]); 
    Rule(neg_term, [zero_term])] in
  (check bool) "parse_disj2" true (result=expected)

let test_parse_conj4 () =
  let result = parse_clause (mother_str ^ " :- " ^ parent_str  ^ ", " ^ woman_str ^ ".") in
  let expected = Rule(mother_term, [parent_term; woman_term]) in
  (check bool) "parse_conj4" true (result=[expected])

let () =
  let open Alcotest in
  run "Clauses' parser tests" [
    "test_parse_fact", [
      test_case "test_parse_fact" `Quick test_parse_fact;
    ];
    "test_parse_rule1", [
      test_case "test_parse_rule" `Quick test_parse_rule1;
    ];
    "test_parse_disj2", [
      test_case "test_parse_disj" `Quick test_parse_disj2;
    ];
    "test_parse_conj4", [
      test_case "test_parse_conj" `Quick test_parse_conj4;
    ];
  ]
