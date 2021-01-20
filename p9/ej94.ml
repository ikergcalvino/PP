open St_tree;;

let is_single t =
  try let _ = branches t in false
  with No_branches -> true;;

let rec breadth_first t = match is_single t with
    true -> [root t]
  | false -> let l, r = branches t in (root t :: (List.append (breadth_first l) (breadth_first r)));;