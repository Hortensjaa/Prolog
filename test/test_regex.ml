open Lib

let test_is_atom () =
  let test_strings = ["test"; "'test'"; "'Some test'"] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_atom" true (Parser.is_atom exp)
  ) test_strings

let test_is_var () =
  let test_strings = ["Test"; "Variable"; "_test"; "_"] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_var" true (Parser.is_var exp)
  ) test_strings

let test_is_number () =
  let test_strings = ["123"; "-305"; "0"; "13789"] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_number" true (Parser.is_number exp)
  ) test_strings

let test_is_comp () =
  let test_strings = 
    ["[1, 2, 3]"; "\"some string\""; "father(John, mary)."; "has_pets(ania, [cat, dog])."; "human(you)."] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_comp" true (Parser.is_comp exp)
  ) test_strings

let test_not_atom () =
  let test_strings = ["not atom for sure"; "_illegalunderscore"; "Uppercase"; "1t1sn0tat0m"] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_atom" false (Parser.is_atom exp)
  ) test_strings

let test_not_var () =
  let test_strings = ["'quotes'"; "lowercase"; "1idk"; "Illegal space"] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_var" false (Parser.is_var exp)
  ) test_strings

let test_not_number () =
  let test_strings = ["a123"; "45oj6"; "0-"; "abcd"] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_number" false (Parser.is_number exp)
  ) test_strings

let test_not_comp () =
  let test_strings = 
    ["[1, 2, 3], 4"; "'it is not string'"; "father(John, mary)"; "has_pets(ania, cat, dog)."; "Human(you)."] in
  List.iter (fun exp ->
    Alcotest.(check bool) "is_comp" false (Parser.is_comp exp)
  ) test_strings

let () =
  let open Alcotest in
  run "Regex tests" [
    "is_atom", [
      test_case "Test is_atom" `Quick test_is_atom;
    ];
    "is_var", [
      test_case "Test is_var" `Quick test_is_var;
    ];
    "is_number", [
      test_case "Test is_number" `Quick test_is_number;
    ];
    "is_comp", [
      test_case "Test is_comp" `Quick test_is_comp;
    ];
    "not_atom", [
      test_case "Test not_atom" `Quick test_not_atom;
    ];
    "not_var", [
      test_case "Test not_var" `Quick test_not_var;
    ];
    "not_number", [
      test_case "Test not_number" `Quick test_not_number;
    ];
    "not_comp", [
      test_case "Test not_comp" `Quick test_not_comp;
    ];
  ]
