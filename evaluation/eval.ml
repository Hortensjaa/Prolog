open Structure.Ast
open! Structure.Print
open Parsing
open Unification
open Refresh
  

let eval query clauses = 

  let print_goals_and_backtrack gs b =
    print_string "goals: ["; 
    List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) gs; 
    print_string "]\n";
    print_string "backtracking: ["; 
    List.iter (fun (id, g) -> (print_string ("("^(string_of_int id)^": "^(term_struct_to_string g)^" ; "))) b; 
    print_string "]\n\n"; in

  let query = Term_parser.parse_term query in 
  let clauses = Parser.parse clauses in
  let all_vars = Hashtbl.create (count_vars 0 query) in
  let start_vars = get_vars query in

  (* main evaluating function *)
  let rec eval_loop (clauses_iter: clause list) (goals: (int * term) list) (backtracking: (int * term) list) =
    match goals with
    | [] ->  (true, backtracking)
    | g_pair::rst_goals ->
      begin match clauses_iter with

      | Fact(f)::clauses_rst -> 
        (try 
          let (_, g) = g_pair in
          let new_g = substitute g all_vars in 
          let new_match_id = List_helpers.find_index (Fact f) clauses in
          
          let (new_f, _) = refresh f [] in
          let vars = unify new_f new_g in 
          let new_backtracking = ((new_match_id, g)::backtracking) in 

          Hashtbl.fold (fun k v () -> Hashtbl.replace all_vars k v) vars ();

           print_endline ("fact nr "^(string_of_int new_match_id)^": "^(term_struct_to_string new_f));
           print_goals_and_backtrack rst_goals new_backtracking;

          eval_loop clauses rst_goals new_backtracking
        with CantUnify -> eval_loop clauses_rst goals backtracking)

      | Rule(hd, bd)::clauses_rst -> 
        (try 
          let (_, g) = g_pair in
          let new_g = substitute g all_vars in 
          let new_match_id = List_helpers.find_index (Rule (hd, bd)) clauses in

          let (new_hd, new_bd) = refresh hd bd in
          let vars = unify new_g new_hd in 
          let args = (List.map (fun term -> (new_match_id, substitute term vars)) new_bd) in

          let new_goals = args@rst_goals in
          let new_backtracking = (new_match_id, g)::backtracking in 

          print_endline ("rule nr "^(string_of_int new_match_id)^": "^(term_struct_to_string new_hd));
          print_goals_and_backtrack new_goals new_backtracking;

          eval_loop clauses new_goals  new_backtracking
        with CantUnify -> eval_loop clauses_rst goals backtracking)

      | [] ->  go_back backtracking goals end 
    
  (* backtracking as seperate function *)
  and go_back b g = 
    print_endline "backtrack!";
    begin match b with
      (* cofamy się - od nowa ewaluujemy ostatnia klauzule z backtracking, a wczesniej usuwamy powiazane z nia cele z goals *)
    | b_prev::b_next::b_rst -> 
      let (prev_num, prev_term) = b_prev in
      let (next_num, _) = b_next in
      let new_goals = (next_num, prev_term)::(List.filter (fun (n, _) -> not (n=prev_num))) g in
      let new_backtracking = b_next::b_rst in
      print_goals_and_backtrack new_goals new_backtracking;

      (try eval_loop (List_helpers.from_nth clauses (prev_num+1)) new_goals new_backtracking
      with List_helpers.OutOfBounds -> (false, b))
      
    | clause_pair::[] -> 
      let (prev_num, prev_term) = clause_pair in
      let new_goals = (-1, prev_term)::(List.filter (fun (n, _) -> not (n=prev_num)) g) in
      print_goals_and_backtrack new_goals [];

      (try eval_loop (List_helpers.from_nth clauses (prev_num+1)) new_goals [] 
      with List_helpers.OutOfBounds -> (false, b))
      
    | [] ->  (false, b) end in

  (* loop to seek solutions as long, as user wants *)
  let rec loop b =
    Hashtbl.clear all_vars; (* todo: tu coś na stówę jest źle - trzeba zapisywać w backtracku podstawienia *)
    let (res, b) = go_back b [] in
    if res then begin 
      (* Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_to_string v)) all_vars;  *)
      Hashtbl.iter (fun k v -> if (List.mem k start_vars) then print_endline (k ^ ": " ^ term_to_string v)) all_vars;
      print_endline "Solution found! Press ';' for more solutions.";
      let user_input = read_line () in
      if user_input = ";" then loop b else true
    end else false in
  
  (* result *)
  let (res, b) = eval_loop clauses [(-1, query)] [] in
  if (res) then begin
    print_endline "Solution found! Press ';' for more solutions.";
    (* Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_to_string v)) all_vars;  *)
    Hashtbl.iter (fun k v -> if (List.mem k start_vars) then print_endline (k ^ ": " ^ term_to_string v)) all_vars; 
    let user_input = read_line () in
    if user_input = ";" then loop b else true end
  else false


    

