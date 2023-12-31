open Alcotest

open Parsing.Parser
open Structure

let test_parse_atom () =
  let result = parse_term "hello" in
  let expected = Atom("hello") in
  (check bool) "is_atom1" true (result=expected)

let test_parse_atom_with_quotes () =
  let result = parse_term "'hello world'" in
  let expected = Atom("'hello world'") in
  (check bool) "is_atom2" true (result=expected)

let test_parse_variable () =
  let result = parse_term "X" in
  let expected = Var("X") in
  (check bool) "is_var" true (result=expected)

let test_parse_number () =
  let result = parse_term "123" in
  let expected = Num(123) in
  (check bool) "is_num" true (result=expected)

let test_parse_comp1arg () =
  let result = parse_term "human(you)." in
  let expected = Comp(Atom("human"), [Atom("you")]) in
  (check bool) "comp1arg" true (result=expected)

let test_parse_comp2args () =
  let result = parse_term "father(John, mary)." in
  let expected = Comp(Atom("father"), [Var("John"); Atom("mary")]) in
  (check bool) "comp2args" true (result=expected)

let test_parse_comp3args () =
  let result = parse_term "employee(Name, Surname, Salary)." in
  let expected = Comp(Atom("employee"), [Var("Name"); Var("Surname"); Var("Salary")]) in
  (check bool) "comp3args" true (result=expected)

let test_parse_comp_with_quotes1 () =
  let result = parse_term "series('Bojack Horseman', 2014)." in
  let expected = Comp(Atom("series"), [Atom("'Bojack Horseman'"); Num(2014)]) in
  (check bool) "comp_with_quotes1" true (result=expected)

let test_parse_comp_with_quotes2 () =
  let result = parse_term "'book of the year'('Watership Down', adams, 1972, 'You try to eat grass that is not there.')." in
  let expected = 
    Comp(Atom("'book of the year'"), [Atom("'Watership Down'"); Atom("adams"); Num(1972); Atom("'You try to eat grass that is not there.'")]) in
  (check bool) "comp_with_quotes2" true (result=expected)

let test_invalid_input1 () =
  let not_term = "invalid input" in
  let test_fn = (fun () -> let _ = (parse_term not_term) in ()) in
  check_raises "parsing invalid input" (Failure ("parse: not a term - "^not_term)) test_fn

let test_invalid_input2 () =
  let not_term = "Father(Adam, Anna)." in
  let test_fn = (fun () -> let _ = (parse_term not_term) in ()) in
  check_raises "parsing invalid input" (Failure ("parse: not a term - "^not_term)) test_fn

let () =
  let open Alcotest in
  run "parser_tests" [
    "parse_atom", [
      test_case "parse atom" `Quick test_parse_atom;
    ];
    "parse_atom_with_quotes", [
      test_case "parse atom_with_quotes" `Quick test_parse_atom_with_quotes;
    ];
    "parse_variable", [
      test_case "parse variable" `Quick test_parse_variable;
    ];
    "parse_number", [
      test_case "parse number" `Quick test_parse_number;
    ];
    "parse_comp1arg", [
      test_case "parse comp1arg" `Quick test_parse_comp1arg;
    ];
    "parse_comp2args", [
      test_case "parse comp2args" `Quick test_parse_comp2args;
    ];
    "parse_comp3args", [
      test_case "parse comp3args" `Quick test_parse_comp3args;
    ];
    "parse_comp_with_quotes1", [
      test_case "test parse comp with quotes1" `Quick test_parse_comp_with_quotes1;
    ];
    "parse_comp_with_quotes2", [
      test_case "test parse comp with quotes2" `Quick test_parse_comp_with_quotes2;
    ];
    "invalid_input1", [
      test_case "invalid input1" `Quick test_invalid_input1;
    ];
    "invalid_input2", [
      test_case "invalid input2" `Quick test_invalid_input2;
    ];
  ]
