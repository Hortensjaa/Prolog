open Structure.Ast
open! Structure.Print
open Parsing
open Unification
  

let eval query clauses = 

  let query = Term_parser.parse_term query in 
  let clauses = Parser.parse clauses in
  let all_vars = Hashtbl.create (count_vars 0 query) in

  let rec eval_loop (clauses_iter: clause list) (goals: (int * term) list) (backtracking: (int * term) list) =
    match goals with
    | [] ->  true
    | g_pair::rst_goals ->
      begin match clauses_iter with
      | Fact(f)::clauses_rst -> 
        (try 
          let (_, g) = g_pair in
          let new_g = substitute g all_vars in 
          let vars = unify f new_g in 
          Hashtbl.fold (fun k v () -> Hashtbl.replace all_vars k v) vars ();
          let new_match_id = List_helpers.find_index (Fact f) clauses in

          print_endline ("fact nr "^(string_of_int new_match_id)^": "^(term_struct_to_string f));
          print_string "goals: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) rst_goals; 
          print_string "]\n";
          print_string "backtracking: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) ((new_match_id, new_g)::backtracking); 
          print_string "]\n\n";

          eval_loop clauses rst_goals ((new_match_id, new_g)::backtracking)
        with CantUnify -> eval_loop clauses_rst goals backtracking)
      
        | Rule(hd, bd)::rst -> 
        (try 
          let (_, g) = g_pair in
          let new_g = substitute g all_vars in 
          let vars = unify new_g hd in 
          let new_match_id = List_helpers.find_index (Rule (hd, bd)) clauses in
          let args = (List.map (fun term -> (new_match_id, substitute term vars)) bd) in

          print_endline ("rule nr "^(string_of_int new_match_id)^": "^(term_struct_to_string hd));
          print_string "goals: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) (args@rst_goals); 
          print_string "]\n";
          print_string "backtracking: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) ((new_match_id, new_g)::backtracking); 
          print_string "]\n\n";

          eval_loop clauses (args@rst_goals) ((new_match_id, new_g)::backtracking)
        with CantUnify -> eval_loop rst goals backtracking)
      | [] ->  
        begin match backtracking with
        | clause_pair::b_rst -> 
          (* cofamy się - od nowa ewaluujemy ostatnia klauzule z backtracking, a wczesniej usuwamy powiazane z nia cele z goals *)
          let (prev_num, prev_term) = clause_pair in
          print_endline ("!backtrack " ^ (string_of_int prev_num) ^ " on clause " ^ (term_struct_to_string prev_term));
          print_string "goals before filter: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) goals; 
          print_string "]\n";
          let new_goals = List.filter (fun (n, _) -> not (n=prev_num)) goals in
          (* let new_goal_from_prev = (, prev_clause) in *)

          print_string "new goals: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) (clause_pair::new_goals); 
          print_string "]\n";
          print_string "new backtrack: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) b_rst; 
          print_string "]\n\n";

          (try eval_loop (List_helpers.from_nth clauses (prev_num+1)) (clause_pair::new_goals) b_rst 
          with List_helpers.OutOfBounds -> false)
          
        | [] ->  false end
       end in
    
    let res = eval_loop clauses [(-1, query)] [] in
    if (res) then Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_to_string v)) all_vars;
    res

