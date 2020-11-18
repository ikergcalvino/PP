let hd =
  function h::_ -> h;;

let tl =
  function _::t -> t;;

let rec length = function
  [] -> 0
  | h::t -> 1 + length t;;

let compare_lengths l1 l2 =
  compare (length l1) (length l2);; 

let rec nth = function l -> function n ->
  if n = 0
    then hd l
    else nth(tl l)(n-1);;

let rec append = function
  [] -> (function l2 -> l2)
  | h::t -> function l2 -> h::append t l2;;

let rec find p l = match l with
  [] -> raise (Not_found)
  | h::t -> (if (p h) then h else find p t);;

let rec for_all p l = match l with
  [] -> true
  | h::t -> (p h) && (for_all p t);;

let rec exists p l = match l with
  [] -> false
  | h::t -> (if (p h) then true else exists p t);;

let rec mem a l = match l with
  [] -> false
  | h::t -> (if a == h then true else mem a t);;

let rec filter p l = match l with
  [] -> []
  | h::t -> (if (p h) then h::(filter p t) else filter p t);;

let find_all = filter;;

let partition p l =
  let pd x (l1, l2) =
    if (p x)
      then (x::l1, l2)
      else (l1, x::l2)
    in fold_right pd l ([], []);;

let rec split  = function
  [] -> ([], [])
  | (h1, h2)::t -> let (l1, l2) = split t in (h1::l1, h2::l2);;

let rec combine l1 l2 = match (l1, l2) with
  ([], []) -> []
  | (h1::t1, h2::t2) -> (h1, h2)::combine t1 t2
  | (_, _) -> raise (Invalid_argument"combine");;

let init n f =
  let rec aux (i, l) =
    if i = n
      then l
      else aux (i + 1, f i::l)
    in rev (aux(0, []));;

let rev_append l1 l2 = append (rev l1) l2;;

let rec concat = function
  [] -> []
  | h::t -> append h (concat t);;

let flatten = concat;;

let rec map f l = match l with
  [] -> []
  | h::t -> f(h)::map f t;;

let rev_map = map;;

let rec map2 f l1 l2 =
  if (length l1 != length l2)
    then raise (Invalid_argument"map2")
    else if (length l1 == 0)
      then []
      else (f (hd l1)(hd l2))::map2 f (tl l1)(tl l2);;

let rec fold_left f a l = match l with
  [] -> a
  | h::t -> fold_left f (f a h) t;;

let rec fold_right f l b = match l with 
  [] -> b
  | h::t -> f h (fold_right f t b);;