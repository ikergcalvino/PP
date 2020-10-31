let comp f g x = f (g x);;
(* comp: ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b) *)

let f = let square x = x * x in comp square ((+) 1);;
(*  *)

f 1, f 2, f 3;;
(*  *)