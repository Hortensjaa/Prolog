type term =
  | VarS of string
  | Num of int
  | Atom of string
  | Comp of term * term list

type term_state = term * bool   (* czy zmienna jest niezanegowana *)
and clause = 
| Fact of term_state
| Rule of term_state * term_state list       (* implication *)

type program = clause list
type query   = term list