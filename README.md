# Prolog
An implementation of Prolog language written in OCaml 5.0.0

## Dependencies

Install dependencies with [OPAM](https://opam.ocaml.org/):
```
$ opam install dune
$ opam install alcotest
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
$ dune exec prolog
```

## Testing

Tests for this project were written with [Alcotest](https://github.com/mirage/alcotest).
To run them, type:
```
$ dune runtest
```