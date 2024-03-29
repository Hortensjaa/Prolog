open Structure.Ast

(* 
  Family tree:
          --- kacper
  olek ---|                     --- ala
          --- ania --- basia ---|
                                --- piotr
*)

let parsed = [
  Fact((Comp(Atom("man"), [Atom("olek")])));
  Fact((Comp(Atom("man"), [Atom("kacper")])));
  Fact((Comp(Atom("man"), [Atom("piotr")])));
  Fact((Comp(Atom("woman"), [Atom("ania")])));
  Fact((Comp(Atom("woman"), [Atom("basia")])));
  Fact((Comp(Atom("woman"), [Atom("ala")])));

  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("kacper")])));
  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("ania")])));
  Fact((Comp(Atom("parent"), [Atom("ania"); Atom("basia")])));
  Fact((Comp(Atom("parent"), [Atom("basia"); Atom("ala")])));
  Fact((Comp(Atom("parent"), [Atom("basia"); Atom("piotr")])));

  Rule(
    (Comp(Atom("father"), [VarS("F"); VarS("C")])),
    [
      (Comp(Atom("man"), [VarS("F")]));
      (Comp(Atom("parent"), [VarS("F"); VarS("C")]))
    ]
  );
  Rule(
    (Comp(Atom("mother"), [VarS("M"); VarS("C")])),
    [
      (Comp(Atom("woman"), [VarS("M")]));
      (Comp(Atom("parent"), [VarS("M"); VarS("C")]))
    ]
  );
  Rule(
    (Comp(Atom("sibling"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("parent"), [VarS("Z"); VarS("X")]));
      (Comp(Atom("parent"), [VarS("Z"); VarS("Y")]));
    ]
  )
]