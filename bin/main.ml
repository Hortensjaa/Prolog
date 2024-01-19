open Evaluation.Backtracking
open Structure.Ast

let mother_term = Comp(Atom("mother"), [VarS("X"); VarS("Y")])
let parent_term = Comp(Atom("parent"), [VarS("X"); VarS("Y")])
let woman_term = Comp(Atom("woman"), [VarS("X")])
let fresh_backtrack = of_list [
  {
    goal = woman_term;
    solution = None;
    last_solution = None;
  };
  {
    goal = parent_term;
    solution = None;
    last_solution = None;
  };
  {
    goal = mother_term;
    solution = None;
    last_solution = None;
  };
]

let first_step = of_lists2 [
  {
    goal = woman_term;
    solution = Some (Comp(Atom("woman"), [Atom("ania")]));
    last_solution = Some 5;
  };][
  {
    goal = parent_term;
    solution = None;
    last_solution = None;
  };
  {
    goal = mother_term;
    solution = None;
    last_solution = None;
  };
] 

let second_step = of_lists2 [
  {
    goal = woman_term;
    solution = Some (Comp(Atom("woman"), [Atom("ania")]));
    last_solution = Some 5;
  };
  {
    goal = parent_term;
    solution = Some (Comp(Atom("parent"), [Atom("ania"); Atom("basia")]));
    last_solution = Some 3;
  };][
  {
    goal = mother_term;
    solution = None;
    last_solution = None;
  };
] 

let () = 
  let result = step_back second_step in
  print_backtrack result;
  print_backtrack fresh_backtrack;
  print_backtrack first_step


