open Structure

(* 
  Family tree with recursive rule:
          --- kacper
  olek ---|
          --- ania --- basia --- piotr
*)

let family_tree3 = 
 "father(olek, kacper).
  father(olek, ania).
  mother(ania, basia).
  mother(basia, piotr).
  parent(X, Y) :- mother(X, Y); father(X, Y).
  ancestor(X, Y) :- parent(X, Y).
  ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)."

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
  );

  Rule(
    (Comp(Atom("ancestor"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("parent"), [VarS("X"); VarS("Y")]), true)
    ]
  );
  Rule(
    (Comp(Atom("ancestor"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("parent"), [VarS("X"); VarS("Z")]), true);
      (Comp(Atom("ancestor"), [VarS("Z"); VarS("Y")]), true)
    ]
  );
]