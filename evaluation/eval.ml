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

let eval ?(read_line=read_line) query clauses = 

  let query = Term_parser.parse_term query in
  let all_vars = Hashtbl.create (count_vars 0 query) in
  let start_vars = get_vars query in

  (* main evaluating function *)
  let rec eval_loop (clauses_iter: clause list) (goals: (int * term) list) (backtracking: step list)  =
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

          eval_loop clauses rst_goals new_backtracking
        with CantUnify -> eval_loop clauses_rst goals backtracking)

      | Rule(hd, bd)::clauses_rst -> 
        (try 
          let (no_g, g) = g_pair in
          let new_g = substitute g all_vars in 
          let new_match_id = List_helpers.find_index (Rule (hd, bd)) clauses in

          let (new_hd, new_bd) = refresh hd bd in
          let vars = unify new_g new_hd in 

          Hashtbl.fold (fun k v () -> Hashtbl.add all_vars k v) vars ();
          let args = (List.map (fun term -> (new_match_id, term)) new_bd) in

          let new_goals = args@rst_goals in
          let new_backtracking = {
            from = no_g;
            index = new_match_id;
            term = g;
            env = Hashtbl.to_seq_keys vars
          }::backtracking in 

          eval_loop clauses new_goals  new_backtracking
        with CantUnify -> eval_loop clauses_rst goals backtracking)

      | [] ->  go_back backtracking goals end 
    
  (* backtracking as seperate function *)
  and go_back b g = 
    begin match b with
    | cur_step::b_rst ->
      let new_goals = (cur_step.from, cur_step.term )::(List.filter (fun (n, _) -> not (n=cur_step.index)) g) in
      Seq.iter (fun k -> Hashtbl.remove all_vars k) cur_step.env;

      (try eval_loop (List_helpers.from_nth clauses (cur_step.index+1)) new_goals b_rst
      with List_helpers.OutOfBounds -> (false, b))
      
    | [] ->  (false, b) end in

  (* loop to seek solutions as long, as user wants *)
  let rec loop res b =
    if res then begin 
      print_endline "--- Solution found! Press ';' to find more solutions or '.' to escape";
      Hashtbl.iter (fun k v -> if (List.mem k start_vars) then print_endline (k ^ ": " ^ term_to_string v)) all_vars;
      print_endline "true";
      let user_input = read_line () in
      if user_input = ";" then let (res, b) = go_back b [] in loop res b 
      else if user_input = "." then true 
      else (print_endline "Choose ';' or '.'"; false) end 
    else false; in
  
  (* result *)
  let (res, b) = eval_loop clauses [(-1, query)] [] in loop res b


    

