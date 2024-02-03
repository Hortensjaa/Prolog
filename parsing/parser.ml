open! Clause_parser


let parse file_name =
  let ic = open_in file_name in

  let rec loop clauses =
    try 
      let line = input_line ic in
      loop (parse_clause line)@clauses
    with _ -> close_in ic; clauses in

  List.rev (loop [])
  