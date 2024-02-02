open Structure.Ast

let rec count_vars counter term =
  match term with
  | VarS(_) -> counter+1
  | Comp(f, args) -> (count_vars counter f) + (List.fold_left (fun acc t -> (count_vars acc t)) 0 args)
  | _ -> counter

let get_vars term =
  let rec loop t lst : string list =
    match t with
    | Atom(_) | Num(_) -> lst
    | VarS(x) -> x::lst
    | Comp(f, args) -> (loop f lst)@(List.fold_left (fun acc arg -> loop arg acc) [] args) in
  loop term []

exception CantUnify
let unify t1 t2 = 
  let vars = Hashtbl.create (count_vars 0 t2) in

  let rec unify_loop t1 t2 =
    match t1, t2 with
    | Atom(v1), Atom(v2) when (v1=v2) -> t1
    | Num(v1), Num(v2)  when (v1=v2)-> t1
    | _, VarS(x) -> 
      Hashtbl.add vars x t1; 
      t2
    | VarS(_), _ -> t1
    | Comp(f1, args1), Comp(f2, args2) when (f1=f2 && List.length args1 = List.length args2) -> Comp(f1, List.map2 unify_loop args1 args2)
    | _ -> raise CantUnify in
    
  let _ = unify_loop t1 t2 in vars

let rec substitute term dict =
  match term with
  | Atom(_) | Num(_) -> term
  | VarS(x) -> 
    (try 
      let sub_term = Hashtbl.find dict x in (substitute sub_term dict)
      with Not_found -> term)
  | Comp(f, args) -> Comp(substitute f dict, List.map (fun t -> substitute t dict) args)
