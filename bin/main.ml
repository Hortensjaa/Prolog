let ht = Hashtbl.create 10


let () = 
  Hashtbl.add ht "1" 2;
  print_endline (string_of_int(Hashtbl.length ht))


