open Structure.Ast

(* more complicated, abstract case 
   source: https://stackoverflow.com/questions/65361713/backtracking-in-prolog*)
 
 let parsed = [
   Fact((Comp(Atom("s"), [Comp(Atom("f"), [Atom("a")])])));
   Fact((Comp(Atom("t"), [Comp(Atom("g"), [Atom("b")])])));
   Fact((Comp(Atom("u"), [Atom("a"); Comp(Atom("g"), [Atom("b")])])));
 
   Rule(
     (Comp(Atom("p"), [VarS("X"); VarS("Y")])),
     [
       (Comp(Atom("q"), [VarS("X"); VarS("Y")]))
     ]
   );
   Rule(
     (Comp(Atom("p"), [VarS("X"); VarS("Y")])),
     [
       (Comp(Atom("r"), [VarS("X"); VarS("Y")]))
     ]
   );
   Rule(
     (Comp(Atom("q"), [VarS("X"); VarS("Y")])),
     [
       (Comp(Atom("s"), [VarS("X")]));
       (Comp(Atom("t"), [VarS("Y")]));
     ]
   );
   Rule(
    (Comp(Atom("r"), [VarS("X"); VarS("Y")])),
    [
      (Comp(Atom("u"), [VarS("X"); VarS("Y")]))
    ]
  );
 ]
