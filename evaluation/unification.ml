open Structure.Ast

let rec count_vars counter term =
  match term with
  | VarS(_) -> counter+1
  | Comp(f, args) -> (count_vars counter f) + (List.fold_left (fun acc t -> (count_vars acc t)) 0 args)
  | _ -> counter

exception CantUnify
let unify q t = 
  let vars = Hashtbl.create (count_vars 0 t) in

  let rec unify_loop q t =
    match q, t with
    | Atom(v1), Atom(v2) when (v1=v2) -> q
    | Num(v1), Num(v2)  when (v1=v2)-> q
    | _, VarS(x) -> 
      Hashtbl.add vars x q; 
      t
    | VarS(_), _ -> q
    | Comp(f1, args1), Comp(f2, args2) when (f1=f2 && List.length args1 = List.length args2) -> Comp(f1, List.map2 unify_loop args1 args2)
    | _ -> raise CantUnify in
    
  let _ = unify_loop q t in vars

let rec substitute term dict =
  match term with
  | Atom(_) | Num(_) -> term
  | VarS(x) -> 
    (try 
      let sub_term = Hashtbl.find dict x in sub_term
      with Not_found -> term)
  | Comp(f, args) -> Comp(f, List.map (fun t -> substitute t dict) args)
