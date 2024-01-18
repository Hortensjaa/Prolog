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

let clauses = 
 "father(olek, kacper).
  father(olek, ania).
  father(olek, asia).
  mother(ania, basia).
  mother(basia, piotr).
  mother(asia, lusia).
  mother(asia, kasia).
  parent(X, Y) :- mother(X, Y); father(X, Y).
  sibling(X, Y) :- parent(Z, X), parent(Z, Y).
  uncle_or_aunt(X, Y) :- sibling(Z, X), parent(Z, Y).
  "

let parsed = [
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("kacper")])), true);
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("ania")])), true);
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("asia")])), true);
  Fact((Comp(Atom("mother"), [Atom("ania"); Atom("basia")])), true);
  Fact((Comp(Atom("mother"), [Atom("basia"); Atom("piotr")])), true);
  Fact((Comp(Atom("mother"), [Atom("asia"); Atom("lusia")])), true);
  Fact((Comp(Atom("mother"), [Atom("asia"); Atom("kasia")])), true);

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
  );
  Rule(
    (Comp(Atom("sibling"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("parent"), [VarS("Z"); VarS("X")]), true);
      (Comp(Atom("parent"), [VarS("Z"); VarS("Y")]), true);
    ]
  );
  Rule(
    (Comp(Atom("uncle_or_aunt"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("sibling"), [VarS("Z"); VarS("X")]), true);
      (Comp(Atom("parent"), [VarS("Z"); VarS("Y")]), true);
    ]
  )
]