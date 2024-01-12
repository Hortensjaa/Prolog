open Structure.Ast

(* 
  Family tree with disjunction:
          --- kacper
  olek ---|
          --- ania --- basia --- piotr
*)

let family_tree3 = 
 "father(olek, kacper).
  father(olek, ania).
  mother(ania, basia).
  mother(basia, piotr).
  parent(X, Y) :- mother(X, Y); father(X, Y)."

let parsed = [
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("kacper")])), true);
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("ania")])), true);
  Fact((Comp(Atom("mother"), [Atom("ania"); Atom("basia")])), true);
  Fact((Comp(Atom("mother"), [Atom("basia"); Atom("piotr")])), true);

  Rule(
    (Comp(Atom("parent"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("mother"), [VarS("X"); VarS("Y")]), true)
    ]
  );
  Rule(
    (Comp(Atom("parent"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("father"), [VarS("X"); VarS("Y")]), true)
    ]
  )
]