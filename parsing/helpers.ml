open Str

open Structure.Ast

let rec parse_term_helper exp =
  let open Regex.Terms in

  if (is_var exp) then VarS(exp)
  else if (is_number exp) then Num(int_of_string exp)
  else if (is_atom exp) then Atom(exp)
  else if (is_compound exp) then 
    let hd = matched_group 1 exp in
    let args = matched_group 2 exp in 
    let args_list = (split (regexp "[, ]+") (String.sub args 1 ((String.length args) - 2))) in

    let rec parse_args_list args_list_cur parsed_list cur_str =
      match args_list_cur with
      | hd::tl -> 
        (* jeśli wyrażenie jest w całości, czyli normalnie się parsuje *)
        (try 
          parse_args_list tl (parsed_list@[(parse_term_helper (cur_str ^ hd))]) ""
        (* jeśli nie da się sparsować, to szukamy dalej poprawnego wyrażenia złożonego *)
        with |_ -> 
          print_endline ("cant parse: " ^ hd);
          parse_args_list tl parsed_list (cur_str ^ hd ^ ", ")) 
      | [] -> parsed_list in

    if (is_atom hd) then Comp((parse_term_helper hd), (parse_args_list args_list [] ""))   
    else failwith "parse: atom is not head"
  else failwith ("parse: " ^ exp ^ " is not a term")


let parse_term exp = 
  let open Regex.Terms in

  if (is_var exp) then failwith "parse: illegal free variable"
  else if (is_number exp) then failwith "parse: illegal free number"
  else parse_term_helper exp


let parse_clause exp =

  let open Regex.Clauses in

  let parse_with_state exp =
    let re = regexp {|\(\\\+ \)?\(.+\)$|}  in
    let b = Str.string_match re exp 0 in
    if (b) then 
      let term = (Str.matched_group 2 exp) in
      try (* if negation *)
        let _ = (Str.matched_group 1 exp) in 
        let parsed = parse_term_helper term in
        (parsed, false)
      with |_ -> (* else: no negation *)
        let parsed = parse_term_helper term in
        (parsed, true) 
    else failwith ("parse_with_state: " ^ exp) in

  if (is_fact exp) then
    let head = matched_group 1 exp in 
    [Fact(parse_term head, true)]

  else if (is_neg exp) then 
    let head = matched_group 1 exp in
    [Fact(parse_term head, false)]

  else if (is_disj exp) then
    let head = matched_group 1 exp in
    let body = matched_group 4 exp in
    let args_list = split (regexp {|\(; \)\|\.|}) body in
    let parsed_head = parse_with_state head in

    List.map (fun arg -> Rule(parsed_head, [parse_with_state arg])) args_list

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
    [Rule(parse_with_state head, List.rev_map (fun arg -> parse_with_state arg) args_list)]

  else if (is_rule exp) then
    let head = matched_group 1 exp in
    let body = matched_group 4 exp in 
    [Rule(parse_with_state head, [parse_with_state body])]
    
  else failwith ("parse: not a clause - " ^ exp)
