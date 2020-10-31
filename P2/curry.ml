(* curry : (('a * 'b) -> 'c) -> ('a -> ('b -> 'c)) *)
let curry =  function f -> function a -> function b -> f (a, b);;

let curry f a b = f (a, b);;

(* uncurry : (('a -> ('b -> 'c) -> ('a * 'b) -> 'c)) *)
let uncurry =  function f -> function (a, b) -> f a b;;

let uncurry f(a, b) = f a b;;

uncurry (+);;
(*  *)

let sum = (uncurry (+));;
(*  *)

sum 1;;
(*  *)

sum (2, 1);;
(*  *)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(*  *)

g (2, 5);;
(*  *)

let h = g 2;;
(*  *)

h 1, h 2, h 3;;
(*  *)