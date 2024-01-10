open Str

open Helpers


let parse clauses =
  let clauses_list = split (regexp "$") clauses in
  List.map parse_clause clauses_list