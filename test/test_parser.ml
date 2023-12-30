open Alcotest

open Parsing.Parser
open Structure

let test_parse_atom () =
  let result = parse_term "hello" in
  let expected = Atom("hello") in
  (check bool) "is_atom" true (result=expected)

let test_parse_variable () =
  let result = parse_term "X" in
  let expected = Var("X") in
  (check bool) "is_var" true (result=expected)

let test_parse_number () =
  let result = parse_term "123" in
  let expected = Num(123) in
  (check bool) "is_num" true (result=expected)

let test_parse_comp1 () =
  let result = parse_term "human(you)." in
  let expected = Comp(Atom("human"), [Atom("you")]) in
  (check bool) "comp1" true (result=expected)

let test_parse_comp2 () =
  let result = parse_term "father(John, mary)." in
  let expected = Comp(Atom("father"), [Var("John"); Atom("mary")]) in
  (check bool) "comp2" true (result=expected)

let test_parse_comp3 () =
  let result = parse_term "employee(Name, Surname, Salary)." in
  let expected = Comp(Atom("employee"), [Var("Name"); Var("Surname"); Var("Salary")]) in
  (check bool) "comp3" true (result=expected)



let test_invalid_input () =
  let test_fn = (fun () -> let _ = (parse_term "invalid input") in ()) in
  check_raises "parsing invalid input" (Failure "parse: not compound") test_fn

let () =
  let open Alcotest in
  run "parser_tests" [
    "parse_atom", [
      test_case "parse atom" `Quick test_parse_atom;
    ];
    "parse_variable", [
      test_case "parse variable" `Quick test_parse_variable;
    ];
    "parse_number", [
      test_case "parse number" `Quick test_parse_number;
    ];
    "parse_comp1", [
      test_case "parse comp1" `Quick test_parse_comp1;
    ];
    "parse_comp2", [
      test_case "parse comp2" `Quick test_parse_comp2;
    ];
    "parse_comp3", [
      test_case "parse comp3" `Quick test_parse_comp3;
    ];
    "invalid_input", [
      test_case "invalid input" `Quick test_invalid_input;
    ];
  ]
