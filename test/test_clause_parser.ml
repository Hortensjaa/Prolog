open Alcotest

open Parsing.Parser
open Structure

let str1 = "type(animation)" 
let str2 = "series('Bojack Horseman', 2014)." 
let term1 = Comp(Atom("type"), [Atom("animation")])
let term2 = Comp(Atom("series"), [Atom("'Bojack Horseman'"); Num(2014)])

let test_parse_fact () =
  let result = parse_clause str2 in
  let expected = Fact(term2) in
  (check bool) "parse_fact" true (result=expected)

let test_parse_neg () =
  let result = parse_clause ("\\+ " ^ str2) in
  let expected = Neg(term2) in
  (check bool) "parse_neg" true (result=expected)

let test_parse_rule () =
  let result = parse_clause (str1 ^ " :- " ^ str2) in
  let expected = Rule(Fact(term1), Fact(term2)) in
  (check bool) "parse_rule" true (result=expected)


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
  ]
