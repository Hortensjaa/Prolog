open Alcotest

open Parsing.Term_parser
open Structure.Ast

let test_parse_atom () =
  let result = parse_term "hello" in
  let expected = Atom("hello") in
  (check bool) "is_atom1" true (result=expected)

let test_parse_variable () =
  let test_fn = (fun () -> let _ = (parse_term "X") in ()) in
  check_raises "parsing free variable" (Failure "parse: illegal free variable") test_fn

let test_parse_number () =
  let test_fn = (fun () -> let _ = (parse_term "123") in ()) in
  check_raises "parsing free variable" (Failure "parse: illegal free number") test_fn

let test_parse_comp1arg () =
  let result = parse_term "human(you)" in
  let expected = Comp(Atom("human"), [Atom("you")]) in
  (check bool) "comp1arg" true (result=expected)

let test_parse_comp2args () =
  let result = parse_term "father(John, mary)" in
  let expected = Comp(Atom("father"), [VarS("John"); Atom("mary")]) in
  (check bool) "comp2args" true (result=expected)

let test_parse_comp3args () =
  let result = parse_term "employee(Name, Surname, Salary)" in
  let expected = Comp(Atom("employee"), [VarS("Name"); VarS("Surname"); VarS("Salary")]) in
  (check bool) "comp3args" true (result=expected)

let test_parse_comp_nested1 () =
  let result = parse_term "a(p(q), f)" in
  let expected = Comp(Atom("a"), [Comp(Atom("p"), [Atom("q")]); Atom("f")]) in
  (check bool) "comp_nested" true (result=expected)

let test_parse_comp_nested2 () =
  let result = parse_term "a(p(q), f(x, y), g(p))" in
  let expected = Comp(Atom("a"), [Comp(Atom("p"), [Atom("q")]); Comp(Atom("f"), [Atom("x"); Atom("y")]); Comp(Atom("g"), [Atom("p")])]) in
  (check bool) "comp_nested" true (result=expected)

let test_invalid_input1 () =
  let not_term = "invalid input" in
  let test_fn = (fun () -> let _ = (parse_term not_term) in ()) in
  check_raises "parsing invalid input" (Failure "parse: invalid input is not a term") test_fn

let test_invalid_input2 () =
  let not_term = "Father(Adam, Anna)." in
  let test_fn = (fun () -> let _ = (parse_term not_term) in ()) in
  check_raises "parsing invalid input" (Failure "parse: Father(Adam, Anna). is not a term") test_fn

let () =
  let open Alcotest in
  run "Terms' parser tests" [
    "parse_atom", [
      test_case "parse atom" `Quick test_parse_atom;
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
    "parse_comp_nested1", [
      test_case "parse comp nested" `Quick test_parse_comp_nested1;
    ];
    "parse_comp_nested2", [
      test_case "parse comp nested" `Quick test_parse_comp_nested2;
    ];
    "invalid_input1", [
      test_case "invalid input1" `Quick test_invalid_input1;
    ];
    "invalid_input2", [
      test_case "invalid input2" `Quick test_invalid_input2;
    ];
  ]
