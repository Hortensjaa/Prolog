let () = 
  let res = (Evaluation.Eval.eval "parent(X, Y)" "parent(ania, basia).") in
  print_endline (if (res) then "true" else "false")


