let is_prm n =
  let rec not_divisible_from d =
    d * d > n || (n mod d <> 0 && not_divisible_from (d+1)) in
  n > 1 && not_divisible_from 2;;

let goldbach n =
  let rec gold_aux d =
    if is_prm d && is_prm (n - d) then (d, n-d)
    else gold_aux (d+1) in
  gold_aux 2;;