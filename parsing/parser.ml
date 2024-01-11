open Str

open Helpers


let parse clauses =
  let clauses_list = split (regexp {|\( .$\)\|\(^ .\)|}) clauses in
  List.iter print_endline clauses_list; 
  List.flatten (List.map parse_clause clauses_list)