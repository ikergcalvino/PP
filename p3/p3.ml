(* Ejercicio 1: *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b);;

(* Ejercicio 2: *)
(* La función is_prm calcula si un nº es primo o no *)
(* (si es divisible o no por algún número anterior) *)
  (* Es una función de tipo : int -> bool = <fun> *)

let is_prm n =
  let rec not_divisible_from d =
    d * d > n || (n mod d <> 0 && not_divisible_from (d+1)) in
  n > 1 && not_divisible_from 2;;
  (* # is_prm 999983;; *)
  (* - : bool = true *)

let is_prm2 n =
  let rec not_divisible_from d =
    (n mod d <> 0 && not_divisible_from (d+1)) || d * d > n in
  n > 1 && not_divisible_from 2;;
  (* # is_prm2 999983;; *)
  (* Stack overflow during evaluation (looping recursion?). *)

(* La desventaja de is_prm2 es que agota la pila por lo tanto *)
    (* para nº elevados como 999983 se vuelve ineficiente *)
            (* por ello la más aconsejable es is_prm *)

(* Ejercicio 3: *)
let capicua n =
  let rec aux n1 n2 =
    if n1 = 0 then n2 = n
    else aux (n1 / 10) (n2 * 10 + n1 mod 10)
  in aux n 0;;