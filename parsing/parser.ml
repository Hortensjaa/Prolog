open Str

open Structure

let rec parse_term exp = 
  let open Regex.Terms in

  if (is_atom exp) then Atom(exp)
  else if (is_var exp) then Var(exp)
  else if (is_number exp) then Num(int_of_string exp)

  else if (is_compound exp) then
    let args_list = (split (regexp "[,(]+") (global_replace (regexp "[ .)]+") "" exp)) in
    match args_list with
    | hd::tl when (is_atom hd) -> 
      let parsed_args = (List.map parse_term tl) in
      Comp(Atom(hd), parsed_args)
    | _ -> failwith "parse: head not atom"
    
  else failwith "parse: not compound"


let rec parse_clause exp =
  let open Regex.Clauses in

  if (is_fact exp) then
    let head = matched_group 1 exp in 
    Fact(parse_term head)

  else if (is_neg exp) then 
    let head = matched_group 1 exp in
    Neg(parse_term head)

  else if (is_rule exp) then
    let head = matched_group 1 exp in
    let body = matched_group 5 exp in 
    Rule((parse_clause (head^".")), (parse_clause (body^".")))

  (* else if (is_conj exp) then
    let head = matched_group 1 exp in
    let body = matched_group 4 exp in
    let args_list = extract_text_helper (Str.full_split (regexp "'") body) [] (regexp "), ") in
    CRule(parse_clause (head^"."), List.map (fun x -> parse_clause (x^").")) args_list) *)

  else failwith ("parse: not a clause - " ^ exp)
