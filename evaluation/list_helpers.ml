
  let find_index elem lst =
    let rec loop i rest_lst =
      match rest_lst with
      | [] -> failwith "find_index: cant find element in list"
      | e::_ when (e=elem)-> i
      | _::rst -> loop (i+1) rst in
      loop 0 lst

  let from_nth lst index =
    let rec loop i rest_lst =
      match rest_lst with
      | [] -> failwith "from_nth: index too big"
      | _ when i=index -> rest_lst
      |_ ::tl -> loop (i+1) tl in
      loop 0 lst

  let to_nth lst index =
    let rec loop i rest_lst res =
      match rest_lst with
      | [] -> failwith "from_nth: index too big"
      | _ when i=index -> List.rev res
      |hd::tl -> loop (i+1) tl (hd::res) in
    loop index lst []
