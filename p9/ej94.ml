open St_tree;;

exception Branches;;

let branches tree = match tree with
    C (tree, d, i) -> (d, i)
  | S h -> raise (Branches);;

let is_single t =
  try let_ = branches t in false
  with Branches -> true;;

let rec breadth_first t = match is_single t with
    true -> [root t]
  | false -> let l, r = branches t
in (root t :: (List.append (breadth_first l) (breadth_first r)));;