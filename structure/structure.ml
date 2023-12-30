type term = 
| Var  of string
| Num  of int
| Atom of string
| Comp of term * term list

type clause = 
| Fact of term
| Rule of term * term list

type program = clause list
type query   = term list