open Str

open Helpers


let parse clauses =
  let clauses_list = split (regexp {|\( .$\)\|\(^ .\)|}) clauses in
  (* List.iter print_endline clauses_list;  *)
  let res = List.flatten (List.map parse_clause clauses_list) in
  (* List.iter (fun x -> (print_endline (Structure.Print.clause_struct_to_string x))) res; *)
  res