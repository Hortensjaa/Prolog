open Structure.Ast

(* 
  Very simple 3-facts clauses list
*)
let parents = 
 "parent(olek, kacper).
  parent(olek, ania).
  parent(hania, kasia)."

let parsed = [
  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("kacper")])), true);
  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("ania")])), true);
  Fact((Comp(Atom("parent"), [Atom("hania"); Atom("kasia")])), true)
]