open Evaluation.Eval

let () = 
  let res = eval "ancestor(olek, basia)" Examples.Ancestors.clauses in 
  print_endline (string_of_bool res);