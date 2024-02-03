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
  let filename = Sys.argv.(1) in
  let clauses = parse filename in main_loop clauses