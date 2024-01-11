open Alcotest

open Parsing.Parser
open Examples

let ex1 = Parents.parents
let ex1res = Parents.parsed
let ex2 = Family_tree1.family_tree1
let ex2res = Family_tree1.parsed
let ex3 = Family_tree2.family_tree2
let ex3res = Family_tree2.parsed

let test_parse_1 () =
  let result = parse ex1 in
  let expected = ex1res in
  (check bool) "parse_1" true (result=expected)

let test_parse_2 () =
  let result = parse ex2 in
  let expected = ex2res in
  (check bool) "parse_2" true (result=expected)

let test_parse_3 () =
  let result = parse ex3 in
  let expected = ex3res in
  (check bool) "parse_3" true (result=expected)

let () =
  run "Parser tests" [
    "test1", [
      test_case "test1" `Quick test_parse_1;
    ];
    "test2", [
      test_case "test2" `Quick test_parse_2;
    ];
    "test3", [
      test_case "test3" `Quick test_parse_3;
    ];
  ]

