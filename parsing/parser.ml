open Str

open Structure


let rec parse_term exp = 
  let open Regex.Terms in

  let rec extract_text splitted to_parse_list =
    match splitted with
    | (Delim _)::(Text txt)::(Delim _)::rest -> 
      extract_text rest (("'"^txt^"'")::to_parse_list)
    | (Text exp)::rest -> 
      let args_list = split (regexp "[ .,()]+") exp in
      extract_text rest (List.rev_append args_list to_parse_list)
    | (Delim _)::[] | [] -> (List.rev to_parse_list)
    | _ -> failwith ("extract_text: not a compound - " ^ exp) in

  if (is_atom exp) then Atom(exp)
  else if (is_var exp) then Var(exp)
  else if (is_number exp) then Num(int_of_string exp)
  else if (is_compound exp) then
    let args_list = extract_text (Str.full_split (Str.regexp "[']") exp) [] in
    match args_list with
    | hd::tl when (is_atom hd) -> 
      let parsed_args = (List.map parse_term tl) in
      Comp(Atom(hd), parsed_args)
    | _ -> failwith ("parse: not a compound - [" ^ String.concat "; " args_list ^ "]")
  else failwith ("parse: not a term - " ^ exp)


(* let parse_clause exp =
  let open Regex.Clauses in
  if (is_fact exp) then
    let head = matched_group 1 exp in 
    Fact(parse_term head)
  else if (is_rule exp) then 
    let head = matched_group 1 exp in 
    let body = matched_group 2 exp in 
    let body_args = (split (regexp {|), |}) body) in
    Fact(parse_term head)
  else failwith ("parse: not a clause - " ^ exp) *)
