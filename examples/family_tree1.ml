open Structure.Ast

(* 
  Family tree:
          --- kacper
  olek ---|
          --- ania --- basia --- piotr
*)

let family_tree1 = 
 "man(olek).
  man(kacper).
  man(piotr).
  woman(ania).
  woman(basia).
  parent(olek, kacper).
  parent(olek, ania).
  parent(ania, basia).
  parent(basia, piotr).
  father(F, C) :- man(F), parent(F, C).
  mother(M, C) :- woman(M), parent(M, C).
  sibling(X, Y) :- parent(Z, X), parent(Z, Y)."

let parsed = [
  Fact((Comp(Atom("man"), [Atom("olek")])), true);
  Fact((Comp(Atom("man"), [Atom("kacper")])), true);
  Fact((Comp(Atom("man"), [Atom("piotr")])), true);
  Fact((Comp(Atom("woman"), [Atom("ania")])), true);
  Fact((Comp(Atom("woman"), [Atom("basia")])), true);

  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("kacper")])), true);
  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("ania")])), true);
  Fact((Comp(Atom("parent"), [Atom("ania"); Atom("basia")])), true);
  Fact((Comp(Atom("parent"), [Atom("basia"); Atom("piotr")])), true);

  Rule(
    (Comp(Atom("father"), [VarS("F"); VarS("C")]), true),
    [
      (Comp(Atom("man"), [VarS("F")]), true);
      (Comp(Atom("parent"), [VarS("F"); VarS("C")]), true)
    ]
  );
  Rule(
    (Comp(Atom("mother"), [VarS("M"); VarS("C")]), true),
    [
      (Comp(Atom("woman"), [VarS("M")]), true);
      (Comp(Atom("parent"), [VarS("M"); VarS("C")]), true)
    ]
  );
  Rule(
    (Comp(Atom("sibling"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("parent"), [VarS("Z"); VarS("X")]), true);
      (Comp(Atom("parent"), [VarS("Z"); VarS("Y")]), true);
    ]
  )
]