type var = term option ref
and term =
  | Var of var          (* zmienna ze wskaznikiem *)
  | VarS of string      (* zmienna na etapie parsowania, przed ustawieniem wskaznikow *)
  | Num of int
  | Atom of string
  | Comp of term * term list

type term_state = term * bool   (* czy zmienna jest niezanegowana *)
and clause = 
| Fact of term_state
| Rule of term_state * term_state         (* implication *)
| CRule of term_state * term_state list   (* implication with conjunction *)
| DRule of term_state * term_state list   (* implication with disjunction *)

type program = clause list
type query   = term list