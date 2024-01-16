open Structure.Ast

(* 
  Family tree - version with negation:
          --- kacper
  olek ---|
          --- ania --- basia --- piotr
*)

let clauses = 
 "woman(ania).
  woman(basia).
  man(X) :- \\+ woman(X).
  parent(olek, kacper).
  parent(olek, ania).
  parent(ania, basia).
  parent(basia, piotr).
  father(F, C) :- man(F), parent(F, C).
  mother(M, C) :- \\+ father(M, C), parent(M, C).
  sibling(X, Y) :- parent(Z, X), parent(Z, Y)."

let parsed = [
  Fact((Comp(Atom("woman"), [Atom("ania")])), true);
  Fact((Comp(Atom("woman"), [Atom("basia")])), true);
  Rule(
    ((Comp(Atom("man"), [VarS("X")])), true),
    [
      ((Comp(Atom("woman"), [VarS("X")])), false)
    ]
  );

  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("kacper")])), true);
  Fact((Comp(Atom("parent"), [Atom("olek"); Atom("ania")])), true);
  Fact((Comp(Atom("parent"), [Atom("ania"); Atom("basia")])), true);
  Fact((Comp(Atom("parent"), [Atom("basia"); Atom("piotr")])), true);

  Rule(
    ((Comp(Atom("father"), [VarS("F"); VarS("C")])), true),
    [
      ((Comp(Atom("man"), [VarS("F")])), true);
      ((Comp(Atom("parent"), [VarS("F"); VarS("C")])), true)
    ]
  );
  Rule(
    ((Comp(Atom("mother"), [VarS("M"); VarS("C")])), true),
    [
      ((Comp(Atom("father"), [VarS("M"); VarS("C")])), false);
      ((Comp(Atom("parent"), [VarS("M"); VarS("C")])), true)
    ]
  );
  Rule(
    ((Comp(Atom("sibling"), [VarS("X"); VarS("Y")])), true),
    [
      ((Comp(Atom("parent"), [VarS("Z"); VarS("X")])), true);
      ((Comp(Atom("parent"), [VarS("Z"); VarS("Y")])), true);
    ]
  );
]