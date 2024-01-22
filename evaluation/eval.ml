open Structure.Ast
open! Structure.Print
open Parsing
open Unification
  

let eval query clauses = 

  let query = Term_parser.parse_term query in 
  let clauses = Parser.parse clauses in
  let all_vars = Hashtbl.create (count_vars 0 query) in

  let rec eval_loop clauses_iter goals =
    match goals with
    | [] ->  true
    | g::rst_goals ->
      begin match clauses_iter with
      | Fact(f)::clauses_rst -> 
        (try 
          let new_g = substitute g all_vars in 
          let vars = unify f new_g in 
          (* print_endline ("fact: " ^ (term_struct_to_string f)); *)
          (* print_endline ("goal: " ^ (term_struct_to_string new_g)); *)
          (* Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_struct_to_string v)) vars; *)
          Hashtbl.fold (fun k v () -> Hashtbl.replace all_vars k v) vars ();
          eval_loop clauses rst_goals
        with CantUnify -> eval_loop clauses_rst goals)
      | Rule(hd, bd)::rst -> 
        (try 
          let new_g = substitute g all_vars in 
          let vars = unify new_g hd in 
          (* print_endline ("rule: " ^ (term_struct_to_string hd)); *)
          (* Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_struct_to_string v)) vars; *)
          let args = (List.map (fun term -> substitute term vars) bd) in
          (* List.iter (fun goal -> (print_string ((term_struct_to_string goal)^"; "))) (args@rst_goals); *)
          (* print_endline ""; *)
          eval_loop clauses (args@rst_goals)
        with CantUnify -> eval_loop rst goals)
      | _ -> false end in
    
    let res = eval_loop clauses [query] in
    if (res) then Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_to_string v)) all_vars;
    res

