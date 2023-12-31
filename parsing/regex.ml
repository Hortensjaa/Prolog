open Str

module Terms = struct
  let is_atom exp = 
    let not_capitalised = regexp {|[a-z][A-Za-z0-9_]*$|} in
    let in_quotes = regexp {|'\(.+\)'$|} in
    string_match not_capitalised exp 0 || string_match in_quotes exp 0

  let is_var exp = 
    let capitalised = regexp {|[A-Z][A-Za-z0-9_]*$|} in
    let underscore_start = regexp {|_[A-Za-z0-9_]*$|} in
    string_match capitalised exp 0 || string_match underscore_start exp 0 
    
  let is_number exp = 
    let re = regexp {|\(-\)?[0-9]+$|}  in
    string_match re exp 0

  let is_list exp = 
    let re = regexp {|\[.*\(, .*\)*\]$|} in
    string_match re exp 0

  let is_string exp = 
    let re = regexp {|".*"$|} in
    string_match re exp 0

  let is_compound exp = 
    let re = regexp {|\(\([a-z][A-Za-z0-9_]*\)\|\('.+'\)\)(\(\([A-Za-z0-9_]+\)\|\('.+'\)\)\(, \(\([A-Za-z0-9_]+\)\|\('.+'\)\)\)*)$|} in
    string_match re exp 0

  let is_term exp =
    is_atom exp || is_var exp || is_number exp || is_compound exp
end

module Clauses = struct
  let is_fact exp = 
    let re = regexp {|\(.+\)\.|} in
    string_match re exp 0 && Terms.is_compound (matched_group 1 exp)

  let is_neg exp = 
    let re = regexp {|\\\+ \(.+\)\.|} in
    string_match re exp 0 && Terms.is_compound (matched_group 1 exp)

  let fact_or_neg exp = is_fact (exp^".") || is_neg (exp^".")

  let is_rule exp =
    let re = regexp {|\(\(\\\+ \)?\(.+\)\) :-\( \\\+\)? \(.+\)\.|} in
    let b = Str.string_match re exp 0 in
    try
      let head = (Str.matched_group 1 exp) in 
      let body = (Str.matched_group 5 exp) in 
      b  && fact_or_neg head && fact_or_neg body
    with |_ -> false

  let is_conj exp =
    let re = regexp {|\(\(\\\+ \)?\(.+\)\) :- \(.+\)\(,\( \\\+\)? \(.+\)\)+\.|} in
    string_match re exp 0 && fact_or_neg (matched_group 1 exp)

  let is_disj exp =
    let re = regexp {|\(\(\\\+ \)?\(.+\)\) :- \(.+\)\(;\( \\\+\)? \(.+\)\)+\.|}in
    string_match re exp 0 && fact_or_neg (matched_group 1 exp)
end
