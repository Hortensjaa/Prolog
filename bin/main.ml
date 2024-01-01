let re = Str.regexp {|\(\(\\\+ \)?\(.+\)\) :- \(\(.+\)\(,\( \\\+\)? \(.+\)\)+\)\.|}
(* let exp = "mother(X, Y) :- parent(X, Y), woman(X)." *)
let exp = "animal(X) :- \\+ plant(X), \\+ fungus(X), alive(X)."
(* let exp = "\\+ alive(X) :- \\+ plant(X), \\+ animal(X)." *)

let () = 
  let b = Str.string_match re exp 0 in
  let head = (Str.matched_group 1 exp) in 
  let body = (Str.matched_group 4 exp) in 
  print_endline head;
  print_endline body;
  print_endline (string_of_bool b);
  (* print_endline (string_of_bool (Parsing.Regex.Clauses.fact_or_neg head));
  print_endline (string_of_bool (Parsing.Regex.Clauses.fact_or_neg body)); *)
  (* List.iter (fun e -> print_endline (e^")")) (Str.split (Str.regexp {|), |}) body) *)


