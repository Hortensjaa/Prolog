open Str

open Regex.Re
open Structure

let rec parse_term exp = 
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