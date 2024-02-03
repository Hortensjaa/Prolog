open Structure.Ast

(* 
  Very simple 3-facts clauses list
*)

let parsed = [
  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("kacper")])));
  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("ania")])));
  Fact((Comp(Atom("parent"), [Atom("hania"); Atom("kasia")])))
]