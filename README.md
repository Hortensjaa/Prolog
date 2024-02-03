# Prolog
An implementation of Prolog language written in OCaml 5.0.0

## Dependencies

Install dependencies with [OPAM](https://opam.ocaml.org/):
```
$ opam install dune
$ opam install alcotest
$ opam install core
```

## Installation

To build this project, clone the repository and build it with dune:
```
$ git clone https://github.com/Hortensjaa/Prolog/
$ cd Prolog
$ dune build
```
to run, all you need is:
```
$ dune exec -- prolog [FILENAME.txt]
```
where in FILENAME you have defined clauses. For example, you can first try
```
$ dune exec -- prolog examples/family_tree1.txt
```

## Testing

Tests for this project were written with [Alcotest](https://github.com/mirage/alcotest) and [ppx_inline_tests](https://github.com/janestreet/ppx_inline_test) from Jane Street.
To run them, type:
```
$ dune runtest
```