let rec divide l = match l with
  h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
  | _ -> l, [];;

let rec merge = function
  [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
                      else h2 :: merge (h1::t1, t2);;

let rec msort1 l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide l in
        merge (msort1 l1, msort1 l2);;