let suml l =
	let rec aux s = function
		  [] -> 0
		| [h] -> s + h
		| h::t -> aux (s + h) t
	in aux 0 l;;

let maxl l = match l with 
  [] -> raise (Failure "maxl")
| h::[] -> h
| h::t -> let rec aux m = function
			[] -> m
			| h::t -> if (m > h);
								then aux m t
								else aux h t
		in aux h l;;

let to0from n = 
	let rec aux l i =
		if i < 0 then List.rev l
		else aux (i::l) (i - 1)
	in aux [] n;;
	
let fromto m n = 
	let rec aux l i =
		if i < m then l
		else aux (i::l) (i - 1)
	in aux [] n;;

let from1to n = 
	let rec aux l i =
		if i < 1 then l
		else aux (i::l) (i - 1)
	in aux [] n;;

let append l1 l2 = match (l1, l2) with
	  [], [] -> []
	| [], _ -> l2
	| _, [] -> l1
  | _, _ -> List.rev_append (List.rev l1) l2;;

let map f l =
	let rec aux f l acc = match l with
		[] -> List.rev acc
		| h::t -> aux f t ((f h)::acc)		
	in aux f l [];;

let power x y =
	let rec innerpower x y acc =
		if y = 0 then acc
		else innerpower x (y - 1) (x * acc)
	in
	if y >= 0 then innerpower x y 1
	else invalid_arg "power";;

let incseg l = 
	let rec aux l acc l2 = match l with
		[] -> []
		| [h] -> List.rev ((h + acc)::l2)
		| h::t -> aux t (h + acc) ((h + acc)::l2)
	in aux l 0 [];;

let rec remove x l = let rec aux acc = function
    [] -> l
    |h::t -> if x = h then List.rev_append acc t
             else aux (h::acc) t 
  in aux [] l ;;

let insert x l =
	let rec aux x l l2 flag = match l, flag with
		_, false -> List.rev_append l2 l
		| [], _ -> List.rev_append  l2 [x]
		| h::t, true -> if x <= h then aux x t (h::x::l2) false
					else aux x t (h::l2) true
	in aux x l [] true;;

let rec insert_gen f x l =
	let rec aux f x l l2 flag = match l,flag with
		  [], _ -> List.rev_append l2 [x]
		| _, false -> List.rev_append l2 l
		| h::t, true -> if f x h then aux f x t (h::x::l2) false
					else aux f x t (h::l2) true
	in aux f x l [] true;;