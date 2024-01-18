open Structure.Ast

(* more complicated, abstract case 
   source: https://stackoverflow.com/questions/65361713/backtracking-in-prolog*)
let clauses = 
  "s(f(a)).
  t(g(b)).
  u(a, g(b)).
  p(X, Y) :- q(X, Y).
  p(X, Y) :- r(X, Y).
  q(X, Y) :- s(X), t(Y).
  r(X, Y) :- u(X, Y)."
 
 let parsed = [
   Fact((Comp(Atom("s"), [Comp(Atom("f"), [Atom("a")])])), true);
   Fact((Comp(Atom("t"), [Comp(Atom("g"), [Atom("b")])])), true);
   Fact((Comp(Atom("u"), [Atom("a"); Comp(Atom("g"), [Atom("b")])])), true);
 
   Rule(
     (Comp(Atom("p"), [VarS("X"); VarS("Y")]), true),
     [
       (Comp(Atom("q"), [VarS("X"); VarS("Y")]), true)
     ]
   );
   Rule(
     (Comp(Atom("p"), [VarS("X"); VarS("Y")]), true),
     [
       (Comp(Atom("r"), [VarS("X"); VarS("Y")]), true)
     ]
   );
   Rule(
     (Comp(Atom("q"), [VarS("X"); VarS("Y")]), true),
     [
       (Comp(Atom("s"), [VarS("X")]), true);
       (Comp(Atom("t"), [VarS("Y")]), true);
     ]
   );
   Rule(
    (Comp(Atom("r"), [VarS("X"); VarS("Y")]), true),
    [
      (Comp(Atom("u"), [VarS("X"); VarS("Y")]), true)
    ]
  );
 ]
