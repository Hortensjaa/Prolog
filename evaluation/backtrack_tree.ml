open Structure.Ast

type node = {
  clause: clause; 
  parent: node option;
  children: node list;
}


let new_tree query = {
  clause = query;
  parent = None;
  children = [];
}



