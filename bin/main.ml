let () = 
  let res = Parsing.Term_parser.parse_term "a(p(q), f)" in
  print_endline "a(p(q), f)";
  print_endline "poprawnie: ";
  print_endline {|Comp(Atom(a), [Comp(Atom(p), [Atom(q)]), Atom(f)])|};
  print_endline "jak bylo: ";
  print_endline (Structure.Print.term_struct_to_string res);


