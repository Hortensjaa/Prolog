open Structure.Ast

(* 
  Family tree with recursive rule:
          --- ania --- basia --- piotr --- kamil
  olek ---|
          --- kacper
*)

let parsed = [
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("kacper")])));
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("ania")])));
  Fact((Comp(Atom("mother"), [Atom("ania"); Atom("basia")])));
  Fact((Comp(Atom("mother"), [Atom("basia"); Atom("piotr")])));
  Fact((Comp(Atom("father"), [Atom("piotr"); Atom("kamil")])));

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
    (Comp(Atom("ancestor"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("parent"), [VarS("X"); VarS("Y")]))
    ]
  );
  Rule(
    (Comp(Atom("ancestor"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("parent"), [VarS("X"); VarS("Z")]));
      (Comp(Atom("ancestor"), [VarS("Z"); VarS("Y")]))
    ]
  );
]