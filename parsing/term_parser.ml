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