open Evaluation.Eval

let () = 
  let res = eval "parent(X, X)" Examples.Family_tree3.clauses in 
  print_endline (string_of_bool res);