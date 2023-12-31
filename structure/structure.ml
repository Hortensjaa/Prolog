type term = 
| Var  of string
| Num  of int
| Atom of string
| Comp of term * term list

type clause = 
| Fact of term
| Negation of term
| Rule of clause * clause         (* implication *)
| CRule of clause * clause list   (* implication with conjunction *)
| DRule of clause * clause list   (* implication with disjunction *)

type program = clause list
type query   = term list