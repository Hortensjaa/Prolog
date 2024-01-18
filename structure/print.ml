open Ast

(* to print like s-expression *)
let rec term_struct_to_string t =

  let body_to_str bd =
    String.concat ", " (List.map term_struct_to_string bd)
  in

  match t with
  | Atom(v) -> ("Atom(" ^ v ^ ")")
  | Num(v) -> ("Number(" ^ string_of_int(v) ^ ")")
  | VarS(v) -> ("Var(" ^ v ^ ")")
  | Comp(hd, bd) -> ("Comp(" ^ term_struct_to_string(hd) ^ ", [" ^ (body_to_str bd) ^ "])")

(* print whole rules *)
let rec clause_struct_to_string c =

  let body_to_str bd =
    String.concat ", " (List.map term_struct_to_string bd)
  in

  match c with
  | Fact(f) -> "Fact(" ^ (term_struct_to_string f) ^ ")"
  | Rule(hd, bd) -> "Rule(" ^ (term_struct_to_string hd) ^ ", [" ^ (body_to_str bd) ^ "])"


(* to print values only *)
let rec term_to_string t = 

  let body_to_str bd =
    String.concat ", " (List.map term_to_string bd)
  in

  match t with
  | Atom(v) | VarS(v) -> v
  | Num(v) -> string_of_int(v)
  | Comp(hd, bd) -> (term_to_string(hd) ^ "(" ^ (body_to_str bd) ^ ")")