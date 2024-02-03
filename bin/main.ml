open! Evaluation.Eval
open! Parsing.Parser

let rec main_loop clauses =
  print_endline "Enter query:";
  let user_input = read_line () in
  (try
    let res = eval user_input clauses in
    print_endline ((string_of_bool res)^"\n");
    main_loop clauses
  with _ -> print_endline "Enter valid query please"; main_loop clauses)


let () = 
  let clauses = parse "examples/ancestors.txt" in main_loop clauses
  (* let res = eval "ancestor(X, kamil)" Examples.Ancestors.clauses in  *)
  (* let res = eval "ancestor(olek, P)" Examples.Ancestors.clauses in  *)
  (* let res = eval "sibling(X, Y)" Examples.Family_tree1.clauses in  *)
  (* let res = eval "mother(X, Y)" Examples.Family_tree1.clauses in  *)
  (* print_endline (string_of_bool res) *)