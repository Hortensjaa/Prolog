open Lib

let () = 
  let string1 = "father(john, mary)." in
  let string2 = "lik_es(apple, [kot, dog])." in
  let string3 = "father(john)." in
  let string4 = "invalidfact(just, checking, syntax)." in
  let string5 = "Mother(ktos)." in
  let string6 = "mother(ktos)" in

print_endline (if Parser.is_comp string1 then "String 1 is a fact" else "String 1 is not a fact");
print_endline (if Parser.is_comp string2 then "String 2 is a fact" else "String 2 is not a fact");
print_endline (if Parser.is_comp string3 then "String 3 is a fact" else "String 3 is not a fact");
print_endline (if Parser.is_comp string4 then "String 4 is a fact" else "String 4 is not a fact");
print_endline (if Parser.is_comp string5 then "String 5 is a fact" else "String 5 is not a fact");
print_endline (if Parser.is_comp string6 then "String 6 is a fact" else "String 6 is not a fact");

