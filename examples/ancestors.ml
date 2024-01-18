open Structure.Ast

(* 
  Family tree with recursive rule:
          --- kacper
  olek ---|
          --- ania --- basia --- piotr
*)

let clauses = 
 "father(olek, kacper).
  father(olek, ania).
  mother(ania, basia).
  mother(basia, piotr).
  parent(X, Y) :- mother(X, Y); father(X, Y).
  ancestor(X, Y) :- parent(X, Y).
  ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)."

let parsed = [
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("kacper")])));
  Fact((Comp(Atom("father"), [Atom("olek"); Atom("ania")])));
  Fact((Comp(Atom("mother"), [Atom("ania"); Atom("basia")])));
  Fact((Comp(Atom("mother"), [Atom("basia"); Atom("piotr")])));

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