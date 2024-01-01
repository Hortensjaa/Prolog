open Alcotest
open Parsing.Regex.Clauses

let test_is_fact () =
  let test_strings = 
    ["human(you)."; "employee(Name, Surname, Salary)."; "fact."] in
  List.iter (fun exp ->
    (check bool) "is_compound" true (is_fact exp)
  ) test_strings

let test_is_neg () =
  let test_strings = 
    ["\\+ human(you)."; "\\+ employee(Name, Surname, Salary)."; "\\+ fact."] in
  List.iter (fun exp ->
    (check bool) "is_compound" true (is_neg exp)
  ) test_strings

let test_is_rule () =
  let test_strings = 
    ["animal(X) :- human(X)."; "odd(X) :- \\+ even(X)."; "\\+ easy :- subject(functional_programming)."] in
  List.iter (fun exp ->
    (check bool) "is_rule" true (is_rule exp)
  ) test_strings

let test_is_conj () =
  let test_strings = 
    ["mother(X, Y) :- parent(X, Y), woman(X)."; "animal(X) :- \\+ plant(X), \\+ fungus(X), alive(X)."; 
    "\\+ song_i_like(X) :- \\+ rock(X), \\+ rap(X)."] in
  List.iter (fun exp ->
    (check bool) "is_conj" true (is_conj exp)
  ) test_strings

let test_is_disj () =
  let test_strings = 
    ["pet(X) :- rat(X); cat(X); dog(X)."; "\\+ mother(X, Y) :- \\+ parent(X, Y); \\+ woman(X)."; "\\+ black :- green; blue; yellow; pink."] in
  List.iter (fun exp ->
    (check bool) "is_disj" true (is_disj exp)
  ) test_strings

let test_not_fact () =
  let test_strings = 
    ["series('Bojack Horseman', 2014)"; "Var(x)."; "employee(Name) :- a(b)."] in
  List.iter (fun exp ->
    (check bool) "is_fact" false (is_fact exp)
  ) test_strings

let test_not_rule () =
  let test_strings = 
    [ "father(John, mary)"; "(has_pets)(ania, cat, dog)."; "Human(you)."; "X."] in
  List.iter (fun exp ->
    (check bool) "is_rule" false (is_rule exp)
  ) test_strings

let test_not_conj () =
  let test_strings = 
    ["\\+ mother(X) :- \\+ parent(X); \\+ woman(X)."; "animal(X) :- \\+ human(X)."; 
    "employee(Name, Surname, Salary)." ;"X :- woman(X), Y."] in
  List.iter (fun exp ->
    (check bool) "is_conj" false (is_conj exp)
  ) test_strings

let test_not_disj () =
  let test_strings = 
    ["mother(X, Y) :- parent(X, Y), woman(X)."; "\\+ human(you)."; "animal(X) :- \\+ human(X).";] in
  List.iter (fun exp ->
    (check bool) "is_disj" false (is_disj exp)
  ) test_strings

let () =
  let open Alcotest in
  run "Regex Clauses tests" [
    "is_fact", [
      test_case "Test is_fact" `Quick test_is_fact;
    ];
    "is_neg", [
      test_case "Test is_neg" `Quick test_is_neg;
    ];
    "is_rule", [
      test_case "Test is_rule" `Quick test_is_rule;
    ];
    "is_conj", [
      test_case "Test is_conj" `Quick test_is_conj;
    ];
    "is_disj", [
      test_case "Test is_disj" `Quick test_is_disj;
    ];
    "not_fact", [
      test_case "Test not_fact" `Quick test_not_fact;
    ];
    "not_rule", [
      test_case "Test not_rule" `Quick test_not_rule;
    ];
    "not_conj", [
      test_case "Test not_conj" `Quick test_not_conj;
    ];
    "not_disj", [
      test_case "Test not_disj" `Quick test_not_disj;
    ];
  ]
