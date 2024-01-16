open Structure.Ast
open Structure.Print
open Parsing


let rec count_vars counter term =
  match term with
  | VarS(_) -> counter+1
  | Comp(f, args) -> (count_vars counter f) + (List.fold_left (fun acc t -> (count_vars acc t)) 0 args)
  | _ -> counter

exception CantUnify
let unify t1 t2 = 
  let vars = Hashtbl.create (count_vars 0 t1) in

  let rec unify_loop t1 t2 =
    match t1, t2 with
    | Atom(v1), Atom(v2) when (v1=v2) -> t1
    | Num(v1), Num(v2)  when (v1=v2)-> t1
      (* przypisanie wartości do zmiennej *)
    | VarS(x), Atom(_) | VarS(x), Num(_) -> 
      Hashtbl.add vars x t2; 
      t2
      (* przypisanie zmiennej do zmiennej - używane przy podstawianiu, żeby później poprawnie się wypisywało *)
    | VarS(_), VarS(x)-> 
      Hashtbl.add vars x t1; 
      t1
    | Comp(f1, args1), Comp(f2, args2) when (f1=f2 && List.length args1 = List.length args2) -> Comp(f1, List.map2 unify_loop args1 args2)
    | _ -> raise CantUnify in
    
  let _ = unify_loop t1 t2 in vars

  

let rec eval query clauses = 

  let rec substitute term dict =
    match term with
    | Atom(_) | Num(_) -> term
    | VarS(x) -> 
      (try 
        let sub_term = Hashtbl.find dict x in sub_term
        with Not_found -> term)
    | Comp(f, args) -> Comp(substitute f dict, List.map (fun t -> substitute t dict) args) in

  let query = Helpers.parse_term query in 
  let clauses = Parser.parse clauses in
  let all_vars = Hashtbl.create (count_vars 0 query) in

  let rec eval_loop q clauses_list =
    match clauses_list with
    | Fact((f, true))::rst -> 
      (try  
        let vars = (unify q f) in    
        Hashtbl.iter (fun k v -> Hashtbl.replace all_vars k v) vars;
        true 
        with CantUnify -> eval_loop q rst)

    | Fact((_, false))::_ -> failwith "not implemented"
    | Rule((hd, true), bd)::rst -> 
      (try  
        let vars = (unify q hd) in 
        let args = (List.map (fun (term, state) -> (substitute term vars, state)) bd) in 
        let results = List.map (fun (term, state) -> if (state) then (eval_loop term clauses) else (not (eval_loop term clauses))) args in
        (try 
          let _ = List.find (fun a -> (a=false)) results in
          false
          with Not_found -> true) 
        with CantUnify -> eval_loop q rst)
    | Rule((_, false), _)::_ -> failwith "not implemented"
    | [] -> false in

  let res = eval_loop query clauses in
  Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_to_string v)) all_vars;
  res

