let rec divide = function
  [] -> [], []
  | h::[] -> [h], []
  | h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2);;

let rec merge ord l1 l2 = match l1, l2 with
  [], l | l, [] -> l
  | h1::t1, h2::t2 -> if ord h1 h2 then h1 :: merge ord t1 l2
                      else h2 :: merge ord l1 t2;;

let rec msort1 ord l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide l in
        merge ord (msort1 ord l1) (msort1 ord l2);;

(* Puede ocurrir Stack Overflow, por ejemplo con: divide [1..256000] *)

let fromto m n =
  let rec auxl m n l =
    if n < m
      then l
      else auxl m (n - 1) (n::l)
  in auxl m n [];;

let l2 = fromto 1 256000;;

let divide' l =
  let rec aux dvd1 dvd2 = function
    [] -> (List.rev dvd1, List.rev dvd2)
    | h::[] -> (List.rev (h::dvd1), List.rev dvd2)
    | h1::h2::t -> aux (h1::dvd1) (h2::dvd2) t
  in aux [] [] l;;

let merge' ord l1 l2 =
  let rec aux mer = function
    [], l | l, [] -> List.rev_append mer l
    | h1::t1, h2::t2 -> if ord h1 h2 then aux (h1::mer) (t1, h2::t2)
                        else aux (h2::mer) (h1::t1, t2)
  in aux [] (l1, l2);;

let rec msort2 ord l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide' l
in merge' ord (msort2 ord l1) (msort2 ord l2);;

(* msort2 es un poco más lento que msort1 *)