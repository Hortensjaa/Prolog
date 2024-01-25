open Structure.Ast
open! Structure.Print
open Parsing
open Unification
open Refresh
  

let eval query clauses = 

  let query = Term_parser.parse_term query in 
  let clauses = Parser.parse clauses in
  let all_vars = Hashtbl.create (count_vars 0 query) in

  (* jest opcja, że trzeba rzeczywiście tworzyć nowe zmienne *)
  let rec eval_loop (clauses_iter: clause list) (goals: (int * term) list) (backtracking: (int * term) list) =
    match goals with
    | [] ->  true
    | g_pair::rst_goals ->
      begin match clauses_iter with
      | Fact(f)::clauses_rst -> 
        (try 
          let (_, g) = g_pair in
          let new_g = substitute g all_vars in 
          let new_match_id = List_helpers.find_index (Fact f) clauses in
          
          let (new_f, _) = refresh f [] in
          let vars = unify new_f new_g in 

          Hashtbl.fold (fun k v () -> Hashtbl.replace all_vars k v) vars ();
          (* Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_to_string v)) vars;

           print_endline ("fact nr "^(string_of_int new_match_id)^": "^(term_struct_to_string new_f));
          print_string "goals: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) rst_goals; 
          print_string "]\n";
          print_string "backtracking: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) ((new_match_id, g)::backtracking); 
          print_string "]\n\n"; *)

          eval_loop clauses rst_goals ((new_match_id, g)::backtracking)
        with CantUnify -> eval_loop clauses_rst goals backtracking)
      
        | Rule(hd, bd)::rst -> 
        (try 
          let (_, g) = g_pair in
          let new_g = substitute g all_vars in 
          let new_match_id = List_helpers.find_index (Rule (hd, bd)) clauses in

          let (new_hd, new_bd) = refresh hd bd in
          let vars = unify new_g new_hd in 
          let args = (List.map (fun term -> (new_match_id, substitute term vars)) new_bd) in

          (* print_endline ("rule nr "^(string_of_int new_match_id)^": "^(term_struct_to_string new_hd));
          print_string "goals: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) (args@rst_goals); 
          print_string "]\n";
          print_string "backtracking: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) ((new_match_id, g)::backtracking); 
          print_string "]\n\n"; *)

          eval_loop clauses (args@rst_goals) ((new_match_id, g)::backtracking)
        with CantUnify -> eval_loop rst goals backtracking)
      | [] ->  
        begin match backtracking with
        | b_prev::b_next::b_rst -> 
          (* cofamy się - od nowa ewaluujemy ostatnia klauzule z backtracking, a wczesniej usuwamy powiazane z nia cele z goals *)
          let (prev_num, prev_term) = b_prev in
          let (next_num, _) = b_next in
          let new_goals = List.filter (fun (n, _) -> not (n=prev_num)) goals in

          (* print_endline ("!backtrack " ^ (string_of_int prev_num) ^ " on clause " ^ (term_struct_to_string prev_term)); 
          print_string "new goals: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) ((next_num, prev_term)::new_goals); 
          print_string "]\n";
          print_string "new backtrack: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) (b_next::b_rst); 
          print_string "]\n\n"; *)

          (try eval_loop (List_helpers.from_nth clauses (prev_num+1)) ((next_num, prev_term)::new_goals) (b_next::b_rst) 
          with List_helpers.OutOfBounds -> false)
          
        | clause_pair::[] -> 
          (* cofamy się - od nowa ewaluujemy ostatnia klauzule z backtracking, a wczesniej usuwamy powiazane z nia cele z goals *)
          let (prev_num, prev_term) = clause_pair in
          let new_goals = List.filter (fun (n, _) -> not (n=prev_num)) goals in

          (* print_endline ("!backtrack " ^ (string_of_int prev_num) ^ " on clause " ^ (term_struct_to_string prev_term));
          print_string "new goals: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) ((-1, prev_term)::new_goals); 
          print_string "]\n";
          print_string "new backtrack: ["; 
          List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) []; 
          print_string "]\n\n"; *)

          (try eval_loop (List_helpers.from_nth clauses (prev_num+1)) ((-1, prev_term)::new_goals) [] 
          with List_helpers.OutOfBounds -> false)
          
        | [] ->  false end
       end in
    
    let res = eval_loop clauses [(-1, query)] [] in
    let start_vars = get_vars query in
    if (res) then Hashtbl.iter (fun k v -> if (List.mem k start_vars) then print_endline (k ^ ": " ^ term_to_string v)) all_vars;
    res

