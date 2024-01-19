open Zipper_list
open Structure.Ast

type node = {
  goal: term;               (* cel do udowodnienia *)
  solution: term option;    (* aktualne rozwiązanie *)
  last_solution: int option (* numer na liście ostatniego rozwiązania *)
}
type backtrack = node zlist

module Node = struct
  open Structure.Print
  let new_node query = {
    goal = query;
    solution = None; 
    last_solution = None
  }

  let solve_node node solution solution_num = {
    goal = node.goal;
    solution = solution;
    last_solution = solution_num
  }

  let clear_solution node = {
    goal = node.goal;
    solution = None;
    last_solution = node.last_solution
  }

  let clear_node node = {
    goal = node.goal;
    solution = None;
    last_solution = None
  }

  let to_string node = 
    "{" ^ (term_struct_to_string node.goal) ^ " | "
    ^ (if (node.solution = None) then "None" else (term_struct_to_string (Option.get node.solution))) ^ " | "
    ^ (if (node.last_solution = None) then "None" else (string_of_int (Option.get node.last_solution))) ^ "}"
end

(* tworzy nową listę backtrackingową z pierwszym celem (zapytaniem) *)
let new_backtrack query: backtrack = zlist_of_list [Node.new_node query]

(* dodaje nowe cele do listy do rozwiązania *)
let add_goals args (backtrack: backtrack): backtrack = append backtrack (List.map Node.new_node args)

(* zwraca aktywny cel, czyli pierwszy za kursorem *)
let active_goal (backtrack: backtrack) = active_elem backtrack

(* krok w przód: 
   - rozwiązanie aktywnego celu 
   - przesunięcie kursora *)
let step_next (solution: term) (solution_num: int) (backtrack: backtrack): backtrack =
  insert (Node.solve_node (active_goal backtrack) (Some solution) (Some solution_num)) (remove backtrack)

(* krok w tył: 
   - usunięcie rozwiązania aktywnego celu
   - (jeśli cel nie jest ostatni na liście - zapomnienie kolejnego)
   - przesunięcie kursora w tył *)
let step_back backtrack: backtrack =
  let backtracked_prev = Node.clear_solution (pred_elem backtrack) in 
  let new_lst = remove_pred backtrack in 
  if (not (zipper_on_start backtrack)) then 
   let backtracked_prev_prev = Node.clear_node (pred_elem new_lst) in
   let newer_lst = insert backtracked_prev_prev (remove_pred new_lst) in 
   move_left (insert backtracked_prev newer_lst) 
  else (move_left (insert backtracked_prev new_lst))

(* konwersja z listy węzłów *)
let of_list (lst: node list): backtrack = zlist_of_list lst

(* konwersja na dwie listy: lewą i prawą *)
let to_lists (backtrack: backtrack) = 
  (List.rev backtrack.left, backtrack.right)

(* konwersja z dwóch list *)
let of_lists2 (left: node list) (right: node list) = {
  left = List.rev left;
  right = right
}

(* printer *)
let print_backtrack backtrack = 
  print_zlist backtrack Node.to_string
