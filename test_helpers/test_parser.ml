open Alcotest

open Parsing.Parser
open Examples

let test_parse_1 () =
  let result = parse "../examples/parents.txt" in
  let expected = Parents.parsed in
  (check bool) "parse_1" true (result=expected)

let test_parse_2 () =
  let result = parse "../examples/family_tree1.txt" in
  let expected = Family_tree1.parsed in
  (check bool) "parse_2" true (result=expected)

let test_parse_4 () =
  let result = parse "../examples/family_tree2.txt" in

  (List.iter (fun c -> print_endline (Structure.Print.clause_struct_to_string c)) result);
  let expected = Family_tree2.parsed in
  (check bool) "parse_4" true (result=expected)

let test_parse_5 () =
  let result = parse "../examples/ancestors.txt" in
  let expected = Ancestors.parsed in
  (check bool) "parse_5" true (result=expected)

let test_parse_6 () =
  let result = parse "../examples/complicated.txt" in
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

