open Alcotest

open Parsing.Parser
open Structure

let str1 = "mother(X, Y)" 
let str2 = "parent(X, Y)"
let str3 = "woman(X)"
let term1 = Comp(Atom("mother"), [Var("X"); Var("Y")])
let term2 = Comp(Atom("parent"), [Var("X"); Var("Y")])
let term3 = Comp(Atom("woman"), [Var("X")])

let test_parse_fact () =
  let result = parse_clause (str2 ^ ".") in
  let expected = Fact(term2) in
  (check bool) "parse_fact" true (result=expected)

let test_parse_neg () =
  let result = parse_clause ("\\+ " ^ str2  ^ ".") in
  let expected = Neg(term2) in
  (check bool) "parse_neg" true (result=expected)

let test_parse_rule () =
  let result = parse_clause (str2 ^ " :- " ^ str1  ^ ".") in
  let expected = Rule(Fact(term2), Fact(term1)) in
  (check bool) "parse_rule" true (result=expected)

let test_parse_conj () =
  let result = parse_clause (str1 ^ " :- " ^ str2  ^ ", " ^ str3 ^ ".") in
  let expected = CRule(Fact(term1), [Fact(term2); Fact(term3)]) in
  (check bool) "parse_conj" true (result=expected)


let () =
  let open Alcotest in
  run "Clauses' parser tests" [
    "test_parse_fact", [
      test_case "test_parse_fact" `Quick test_parse_fact;
    ];
    "test_parse_neg", [
      test_case "test_parse_neg" `Quick test_parse_neg;
    ];
    "test_parse_rule", [
      test_case "test_parse_rule" `Quick test_parse_rule;
    ];
    "test_parse_conj", [
      test_case "test_parse_conj" `Quick test_parse_conj;
    ];
  ]
