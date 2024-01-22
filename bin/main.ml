open Evaluation.Eval

let () = 
  let res = eval "mother(A, B)" Examples.Family_tree1.clauses in 
  print_endline (string_of_bool res);