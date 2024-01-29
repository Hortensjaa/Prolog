open Evaluation.Eval

let () = 
  let res = eval "ancestor(X, kamil)" Examples.Ancestors.clauses in 
  (* let res = eval "mother(X, Y)" Examples.Family_tree1.clauses in  *)
  print_endline (string_of_bool res);