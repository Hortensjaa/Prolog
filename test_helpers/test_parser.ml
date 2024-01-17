open Alcotest

open Parsing.Parser
open Examples

let test_parse_1 () =
  let result = parse Parents.clauses in
  let expected = Parents.parsed in
  (check bool) "parse_1" true (result=expected)

let test_parse_2 () =
  let result = parse Family_tree1.clauses in
  let expected = Family_tree1.parsed in
  (check bool) "parse_2" true (result=expected)

let test_parse_3 () =
  let result = parse Family_tree2.clauses in
  let expected = Family_tree2.parsed in
  (check bool) "parse_3" true (result=expected)

let test_parse_4 () =
  let result = parse Family_tree3.clauses in
  let expected = Family_tree3.parsed in
  (check bool) "parse_4" true (result=expected)

let test_parse_5 () =
  let result = parse Ancestors.clauses in
  let expected = Ancestors.parsed in
  (check bool) "parse_5" true (result=expected)

let test_parse_6 () =
  let result = parse Complicated.clauses in
  let expected = Complicated.parsed in
  (check bool) "parse_6" true (result=expected)

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
    "test4", [
      test_case "test4" `Quick test_parse_4;
    ];
    "test5", [
      test_case "test5" `Quick test_parse_5;
    ];
    "test6", [
      test_case "test6" `Quick test_parse_6;
    ];
  ]

