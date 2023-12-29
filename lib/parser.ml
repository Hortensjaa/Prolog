open Str

let not_capitalised = regexp {|[a-z][A-Za-z0-9_]*$|}
let in_quotes = regexp {|'.+'$|}
let capitalised = regexp {|[A-Z][A-Za-z0-9_]*$|}
let underscore_start = regexp {|_[A-Za-z0-9_]*$|}
let is_num = regexp {|\(-\)?[0-9]+$|}
let is_list = regexp {|\[.*\(, .*\)*\]$|}
let is_string = regexp {|".*"$|}
let is_fact = regexp {|\([a-z][A-Za-z0-9_]*\)([A-Za-z0-9_]+\(, \(\([A-Za-z0-9_]+\)\|\(\[.*\(, .*\)*\]\)\)\)?)\.$|}
let is_atom exp = (string_match not_capitalised exp 0) || (string_match in_quotes exp 0) 
let is_var exp = (string_match capitalised exp 0) || (string_match underscore_start exp 0) 
let is_number exp = (string_match is_num exp 0)
let is_comp exp = (string_match is_list exp 0) || (string_match is_string exp 0) || (string_match is_fact exp 0)
