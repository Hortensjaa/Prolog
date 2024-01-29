open Structure.Ast
open! Structure.Print
open Parsing
open Unification
open Refresh
  
type step = {
  from: int;
  index: int;
  term: term;
  env: string Seq.t
}

let eval query clauses = 

  let query = Term_parser.parse_term query in 
  let clauses = Parser.parse clauses in
  let all_vars = Hashtbl.create (count_vars 0 query) in
  let start_vars = get_vars query in

  let print_goals_and_backtrack_and_env gs b =
    print_string "goals: ["; 
    List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) gs; 
    print_string "]\n";
    print_string "backtracking: ["; 
    List.iter (fun b -> (print_string ("("^(string_of_int b.from)^" -> "^(string_of_int b.index)^": "^(term_struct_to_string b.term)^" ; "))) b; 
    print_string "]\n"; 
    print_string "env: {"; Hashtbl.iter (fun k v -> print_string (k ^ ": " ^ term_struct_to_string v ^ ", ")) all_vars; print_endline "}\n";in

  (* main evaluating function *)
  let rec eval_loop (clauses_iter: clause list) (goals: (int * term) list) (backtracking: step list) =
    match goals with
    | [] ->  (true, backtracking)
    | g_pair::rst_goals ->
      begin match clauses_iter with

      | Fact(f)::clauses_rst -> 
        (try 
          let (no_g, g) = g_pair in
          let new_g = substitute g all_vars in 
          let new_match_id = List_helpers.find_index (Fact f) clauses in
          
          let (new_f, _) = refresh f [] in
          let vars = unify new_f new_g in 

          Hashtbl.fold (fun k v () -> Hashtbl.add all_vars k v) vars ();
          let new_backtracking = {
            from = no_g;
            index = new_match_id;
            term = g;
            env = Hashtbl.to_seq_keys vars
          }::backtracking in 

          print_endline ("fact nr "^(string_of_int new_match_id)^": "^(term_struct_to_string new_f));
          print_goals_and_backtrack_and_env rst_goals new_backtracking;

          eval_loop clauses rst_goals new_backtracking
        with CantUnify -> eval_loop clauses_rst goals backtracking)

      | Rule(hd, bd)::clauses_rst -> 
        (try 
          let (no_g, g) = g_pair in
          let new_g = substitute g all_vars in 
          let new_match_id = List_helpers.find_index (Rule (hd, bd)) clauses in

          let (new_hd, new_bd) = refresh hd bd in
          let vars = unify new_g new_hd in 
          let args = (List.map (fun term -> (new_match_id, substitute term vars)) new_bd) in

          let new_goals = args@rst_goals in
          let new_backtracking = {
            from = no_g;
            index = new_match_id;
            term = g;
            env = Seq.empty
          }::backtracking in 

          print_endline ("rule nr "^(string_of_int new_match_id)^": "^(term_struct_to_string new_hd));
          print_goals_and_backtrack_and_env new_goals new_backtracking;

          eval_loop clauses new_goals  new_backtracking
        with CantUnify -> eval_loop clauses_rst goals backtracking)

      | [] ->  go_back backtracking goals end 
    
  (* backtracking as seperate function *)
  and go_back b g = 
    print_endline "backtrack!";
    begin match b with
      (* cofamy się - od nowa ewaluujemy pierwsza klauzule z backtracking, a wczesniej usuwamy powiazane z nia cele z goals *)
    | b_prev::b_rst ->
      let cur_clause = b_prev.term in
      let new_goals = (b_prev.from, cur_clause)::(List.filter (fun (n, _) -> not (n=b_prev.index)) g) in (*to się wydaje ryzykowne*)
      Seq.iter (fun k -> Hashtbl.remove all_vars k) b_prev.env;

      print_goals_and_backtrack_and_env new_goals b_rst;
      (try eval_loop (List_helpers.from_nth clauses (b_prev.index+1)) new_goals b_rst
      with List_helpers.OutOfBounds -> (false, b))
      
    | [] ->  (false, b) end in

  (* loop to seek solutions as long, as user wants *)
  let rec loop b =
    let (res, b) = go_back b [] in
    if res then begin 
      print_endline "--- Solution found! Press ';' for more solutions.";
      Hashtbl.iter (fun k v -> if (List.mem k start_vars) then print_endline (k ^ ": " ^ term_to_string v)) all_vars;
      let user_input = read_line () in
      if user_input = ";" then loop b else true
    end else false in
  
  (* result *)
  let (res, b) = eval_loop clauses [(-1, query)] [] in
  if (res) then begin
    print_endline "--- Solution found! Press ';' for more solutions.";
    Hashtbl.iter (fun k v -> if (List.mem k start_vars) then print_endline (k ^ ": " ^ term_to_string v)) all_vars; 
    let user_input = read_line () in
    if user_input = ";" then loop b else true end
  else false


    

