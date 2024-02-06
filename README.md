# Prolog
An implementation of Prolog language written in OCaml 5.0.0

## Dependencies

Install dependencies with [OPAM](https://opam.ocaml.org/):
```
$ opam install dune
$ opam install alcotest
$ opam install core
```

## Implementation

### Input data
List of clauses needs to be stored as a list in .txt file - every clause in seperate line and ended with ".". Example:
```
parent(alice, bob).
parent(bob, charlie).
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
```

### Parser
Parser is implemented with [Str](https://v2.ocaml.org/api/Str.html) library. It uses regular expressions to match clauses with patterns.

### Evalution
The main idea behind implemented evaluation algorithm is using stack of active goals and stack of already solved goals to enable backtracking, while iterating over clauses list. 
* unifying goal with fact solves it
* unifying goal with rule's head solves it and adds rule's arguments to active goals
* when current goal can't be unified with any clause on the list, algorithm backtracks
* backtrack means "unsolving" (removing from solved goals and adding to active goals) newest record and removing goals connected with it
* memorizing clauses' numbers let as iterate starting from next clause while backtracking.

## Usage
### Installation
To build this project, clone the repository and build it with dune:
```
$ git clone https://github.com/Hortensjaa/Prolog/
$ cd Prolog
$ dune build
```

### Execution
to run, all you need is:
```
$ dune exec -- prolog [FILENAME.txt]
```
where in FILENAME you have defined clauses. For example, you can try:
```
$ dune exec -- prolog examples/family_tree1.txt
```

### Using
Query clauses list by entering query and submit it with "enter" button. While more solutions can be found, you can enter ";" to find next or "." to evaluate next query.

## Testing

Tests for this project were written with [Alcotest](https://github.com/mirage/alcotest) and [ppx_inline_tests](https://github.com/janestreet/ppx_inline_test) from Jane Street.
To run them, type:
```
$ dune runtest
```