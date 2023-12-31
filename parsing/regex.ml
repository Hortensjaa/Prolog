open Str

module Re = struct
  let not_capitalised = regexp {|[a-z][A-Za-z0-9_]*$|}
  let in_quotes = regexp {|'\(.+\)'$|}
  let capitalised = regexp {|[A-Z][A-Za-z0-9_]*$|}
  let underscore_start = regexp {|_[A-Za-z0-9_]*$|}
  let num = regexp {|\(-\)?[0-9]+$|}
  let list = regexp {|\[.*\(, .*\)*\]$|}
  let string = regexp {|".*"$|}
  let compound = regexp {|\([']?[a-z][A-Za-z0-9_ ]*[']?\)(\([A-Za-z0-9_' ]+\)\(, \(\([A-Za-z0-9_' .]+\)\|\(\[.*\(, .*\)*\]\)\)\)*)\.$|}

  let is_atom exp = (string_match not_capitalised exp 0) || (string_match in_quotes exp 0) 
  let is_var exp = (string_match capitalised exp 0) || (string_match underscore_start exp 0) 
  let is_number exp = (string_match num exp 0)
  let is_list exp = (string_match list exp 0) 
  let is_string exp = (string_match string exp 0) 
  let is_compound exp = (string_match compound exp 0) 
end