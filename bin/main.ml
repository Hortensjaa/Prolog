(* let exp = "human(x) :- man(x, y)."
(* let exp = "human(x) :- man(x, y), woman(x), maaansd('dio', djds, dsd)." *)
  
(* let re = Str.regexp {|\(.+\) :- \(.+\)\([,;] \(.+\)\)*\.|}   *)
let re = Str.regexp {|\(.+\) :- \(.+\)\.|} *)
let re = Str.regexp {|\(\(\\\+ \)?\(.+\)\) :-\( \\\+\)? \(.+\)\.|} 
let exp = "human(x) :- man(x, y)."

let () = 
  (* print_endline (string_of_bool (Parsing.Regex.Clauses.is_rule "djksndsjkndskj")); *)
  let b = Str.string_match re exp 0 in
  let head = (Str.matched_group 1 exp) in 
  let body = (Str.matched_group 5 exp) in 
  print_endline head;
  print_endline body;
  print_endline (string_of_bool (Parsing.Regex.Clauses.is_rule exp));
  print_endline (string_of_bool b);
  print_endline (string_of_bool (Parsing.Regex.Clauses.fact_or_neg head));
  print_endline (string_of_bool (Parsing.Regex.Clauses.fact_or_neg body));
  (* List.iter (fun e -> print_endline (e^")")) (Str.split (Str.regexp {|), |}) body) *)


