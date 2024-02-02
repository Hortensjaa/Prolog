open Evaluation.Eval

let rec main_loop () =
  print_endline "Enter query:";
  let user_input = read_line () in
  (try
    let res = eval user_input Examples.Ancestors.clauses in
    print_endline ((string_of_bool res)^"\n");
    main_loop ()
  with _ -> print_endline "Enter valid query please"; main_loop ())


let () = main_loop ()
  (* let res = eval "ancestor(X, kamil)" Examples.Ancestors.clauses in  *)
  (* let res = eval "ancestor(olek, P)" Examples.Ancestors.clauses in  *)
  (* let res = eval "sibling(X, Y)" Examples.Family_tree1.clauses in  *)
  (* let res = eval "mother(X, Y)" Examples.Family_tree1.clauses in  *)
  (* print_endline (string_of_bool res) *)