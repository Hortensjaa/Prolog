open Structure.Ast
open Structure.Print
open Parsing

exception CantUnify
let unify t1 t2 = 
  let vars = Hashtbl.create 10 in

  let rec unify_loop t1 t2 =
    match t1, t2 with
    | Atom(v1), Atom(v2) when (v1=v2) -> t1
    | Num(v1), Num(v2)  when (v1=v2)-> t1
    | VarS(x), t -> 
      Hashtbl.add vars x t; 
      t
    | t, VarS(_)-> t  (*???*)
    | Comp(h1, b1), Comp(h2, b2) when (h1=h2 && List.length b1 = List.length b2) -> Comp(h1, List.map2 unify_loop b1 b2)
    | _ -> raise CantUnify in
    
  let _ = unify_loop t1 t2 in vars

let eval exp clauses = 

  let exp = Helpers.parse_term exp in 
  let clauses = Parser.parse clauses in

  let rec eval_loop clauses_list =
    match clauses_list with
    | Fact((f, true))::rst -> 
      (try  
        let vars = (unify exp f) in 
        Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ term_to_string v)) vars;
        true 
        with CantUnify -> eval_loop rst)
    | Fact((_, false))::_ -> failwith "not implemented"
    | Rule((_, true), _)::_ -> failwith "not implemented"
    | Rule((_, false), _)::_ -> failwith "not implemented"
    | [] -> false in

  eval_loop clauses

