open Structure.Ast

(* 
  Bigger family tree with disjunction:
           --- kacper
           |
  olek --- |--- ania --- basia --- piotr
           |
           |            --- lusia
           --- asia --- |
                        --- kasia
*)

let parsed = [
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("kacper")])));
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("ania")])));
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("asia")])));
  Fact((Comp(Atom("mother"), [Atom("ania"); Atom("basia")])));
  Fact((Comp(Atom("mother"), [Atom("basia"); Atom("piotr")])));
  Fact((Comp(Atom("mother"), [Atom("asia"); Atom("lusia")])));
  Fact((Comp(Atom("mother"), [Atom("asia"); Atom("kasia")])));

  Rule(
    (Comp(Atom("parent"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("mother"), [VarS("X"); VarS("Y")]))
    ]
  );
  Rule(
    (Comp(Atom("parent"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("father"), [VarS("X"); VarS("Y")]))
    ]
  );
  Rule(
    (Comp(Atom("sibling"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("parent"), [VarS("Z"); VarS("X")]));
      (Comp(Atom("parent"), [VarS("Z"); VarS("Y")]));
    ]
  );
  Rule(
    (Comp(Atom("uncle_or_aunt"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("sibling"), [VarS("Z"); VarS("X")]));
      (Comp(Atom("parent"), [VarS("Z"); VarS("Y")]));
    ]
  )
]