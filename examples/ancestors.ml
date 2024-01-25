open Structure.Ast

(* 
  Family tree with recursive rule:
          --- ania --- basia --- piotr
  olek ---|
          --- kacper
*)

let clauses = 
 "mother(ania, basia).
  mother(basia, piotr).
  father(olek, ania).
  father(olek, kacper).
  parent(X, Y) :- mother(X, Y); father(X, Y).
  ancestor(X, Y) :- parent(X, Y).
  ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)."

let parsed = [
  Fact((Comp(Atom("mother"), [Atom("ania"); Atom("basia")])));
  Fact((Comp(Atom("mother"), [Atom("basia"); Atom("piotr")])));
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("ania")])));
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("kacper")])));

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