type 'a zlist = {
  left: 'a list;
  right: 'a list;
}

let zlist_of_list lst = { 
  left = []; 
  right = lst 
}

let to_list zlst = List.rev_append zlst.left zlst.right

(* pierwszy element za kursorem *)
let active_elem zlst =
  match zlst.right with
  | [] -> failwith "no active node"
  | hd :: _ -> hd

(* element przed kursorem *)
let pred_elem zlst = 
  match zlst.left with
  | [] -> failwith "no previous elem - zipper on root"
  | hd::_ -> hd

let move_left zlst =
  match zlst.left with
  | [] -> zlst
  | hd :: tl -> { left = tl; right = hd :: zlst.right }

let move_right zlst =
  match zlst.right with
  | [] -> zlst
  | hd :: tl -> { left = hd :: zlst.left; right = tl }

(* dodawanie elementu przed kursor *)
let insert x zlst = { 
  left = x :: zlst.left; 
  right = zlst.right 
}

(* usuniecie elementu za kursorem *)
let remove zlst =
  match zlst.right with
  | [] -> zlst
  | _ :: tl -> { left = zlst.left; right = tl }

(* usunięcie elementu przed kursorem *)
let remove_pred zlst = 
  match zlst.left with
  | [] -> zlst
  | _ :: tl -> { left = tl; right = zlst.right }

(* dodanie nowych celów na początek *)
let append zlst new_elems = {
  left = zlst.left;
  right = (List.rev new_elems)@zlst.right
}

(* czy kursor jest na końcu listy celów *)
let zipper_on_start zlist =
  match zlist.left with
  | _::[] -> true
  | _ -> false 

(* converting to string *)
let to_string zlst to_str_fun = 
  let rec loop lst_rest str = 
    match lst_rest with
    | [] -> str
    | e::rst -> loop rst ((to_str_fun e) ^ " ") in
  let left = loop (List.rev zlst.left) "" in
  let right = loop (zlst.right) "" in
  left ^ "| " ^ right

(* printer *)
let print_zlist zlst to_str_fun = 
  let rec print_list = function 
    | [] -> ()
    | e::l -> print_endline (to_str_fun e); print_list l
  in
  begin
    print_list (List.rev zlst.left);
    print_string " -----------------------------\n ";
    print_list zlst.right;
    print_endline "";
  end ;;