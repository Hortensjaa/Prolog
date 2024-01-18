open Str

open Structure.Ast
open Term_parser

let parse_clause exp =

  let open Regex.Clauses in

  if (is_fact exp) then
    let head = matched_group 1 exp in 
    [Fact(parse_term head)]

  else if (is_disj exp) then
    let head = matched_group 1 exp in
    let body = matched_group 2 exp in
    let args_list = split (regexp {|\(; \)\|\.|}) body in
    let parsed_head = parse_term head in

    List.map (fun arg -> Rule(parsed_head, [parse_term_helper arg])) args_list

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
    let body = matched_group 2 exp in
    let args_list = conj_parse_helper (full_split (regexp {|[()]\|\(, \)|}) body) [] in
    [Rule(parse_term head, List.rev_map (fun arg -> parse_term_helper arg) args_list)]

  else if (is_rule exp) then
    let head = matched_group 1 exp in
    let body = matched_group 4 exp in 
    [Rule(parse_term head, [parse_term_helper body])]
    
  else failwith ("parse: not a clause - " ^ exp)