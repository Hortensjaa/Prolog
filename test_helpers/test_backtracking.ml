open Alcotest

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

let test_add_goals () =
  let result = add_goals [parent_term; woman_term] (new_backtrack mother_term) in
  let expected = fresh_backtrack in
  (check bool) "test_add_goals" true (result=expected)

let test_active_goal () =
  let result = active_goal fresh_backtrack in
  let expected = {
    goal = woman_term;
    solution = None;
    last_solution = None;
  } in
  (check bool) "test_active_goal" true (result=expected)

let test_step_next1 () =
  let result = step_next (Comp(Atom("woman"), [Atom("ania")])) 5 fresh_backtrack in
  let expected = first_step in
  (check bool) "test_step_next2" true (result=expected)

let test_step_next2 () =
  let result = step_next (Comp(Atom("parent"), [Atom("ania"); Atom("basia")])) 3 first_step in
  let expected = second_step in
  (check bool) "test_step_next" true (result=expected)

let test_step_back1 () =
  let result = step_back first_step in
  let expected =  of_list [
    {
      goal = woman_term;
      solution = None;
      last_solution = Some 5;
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
  ] in
  (check bool) "test_step_back" true (result=expected)

let test_step_back2 () =
  let result = step_back second_step in
  let expected =  of_lists2 [
    {
      goal = woman_term;
      solution = None;
      last_solution = None;
    }; ][
    {
      goal = parent_term;
      solution = None;
      last_solution = Some 3;
    };
    {
      goal = mother_term;
      solution = None;
      last_solution = None;
    };
  ] in
  (check bool) "test_step_back" true (result=expected)


let () =
  run "Backtracking tests" [
    "test_add_goals", [
      test_case "test_add_goals" `Quick test_add_goals;
    ];
    "test_active_goal", [
      test_case "test_active_goal" `Quick test_active_goal;
    ];
    "test_step_next1", [
      test_case "test_step_next" `Quick test_step_next1;
    ];
    "test_step_next2", [
      test_case "test_step_next" `Quick test_step_next2;
    ];
    "test_step_back1", [
      test_case "test_step_next" `Quick test_step_back1;
    ];
    "test_step_back2", [
      test_case "test_step_next" `Quick test_step_back2;
    ];
  ]

