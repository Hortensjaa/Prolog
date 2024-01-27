open Evaluation.Eval

let () = 
  let res = eval "ancestor(X, piotr)" Examples.Ancestors.clauses in 
  print_endline (string_of_bool res);