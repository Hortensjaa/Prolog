open Structure.Ast

let counter = ref 0

let rec refresh_term t =
  match t with
  | Atom(_)| Num(_)-> t
  | VarS(x) -> VarS (x^(string_of_int !counter))
  | Comp(f, args) -> Comp(refresh_term f, List.map refresh_term args)

let refresh f args =
  counter := !counter+1;
  (refresh_term f, List.map refresh_term args)