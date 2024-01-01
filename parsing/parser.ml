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
    | _ -> failwith "parse: something went very wrong"

  else failwith "parse: not a term"


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

  else if (is_disj exp) then
    let head = matched_group 1 exp in
    let body = matched_group 4 exp in
    let args_list = split (regexp {|\(; \)\|\.|}) body in
    DRule((parse_clause (head^".")), List.map (fun arg -> parse_clause (arg^".")) args_list)

  else if (is_conj exp) then
    let rec conj_parse_helper splitted_exp res =
      match splitted_exp with
      | Text(atom)::Delim(", ")::rest -> conj_parse_helper rest (atom::res)
      | Text(atom)::[] -> atom::res
      | Text(functator)::Delim("(")::rest -> 
        
        let rec loop rst res_strin = 
          begin match rst with
          | Text(last_arg)::Delim(")")::Delim(", ")::rest 
          | Text(last_arg)::Delim(")")::rest -> conj_parse_helper rest ((res_strin^last_arg^")")::res)
          | Text(arg)::Delim(", ")::rest -> loop rest (res_strin^arg^", ")
          | _ -> failwith "conj loop: probably not a compound term"
          end
        in loop rest (functator^"(")
  
      | _ -> res
    in

    let head = matched_group 1 exp in
    let body = matched_group 4 exp in
    let args_list = conj_parse_helper (full_split (regexp {|[()]\|\(, \)|}) body) [] in
    CRule((parse_clause (head^".")), List.rev_map (fun arg -> parse_clause (arg^".")) args_list)
    
  else failwith ("parse: not a clause - " ^ exp)
