open St_tree;;

exception Branches;;

let branches tree = match tree with
    C (tree, d, i) -> (d, i)
  | S h -> raise (Branches);;

let is_single t =
  try let_ = branches t in false
  with Branches -> true;;
(*
let breadth_first arbol =
  let rec aux i = function
      S -> []
    | C(raiz, S, S) -> [(raiz, i)]
    | C(raiz, rama1, rama2) -> ((raiz, i)::(aux (i+1) rama1)) @ (aux (i+1) rama2)
in List.map (fun (a, b) -> a)
            (List.stable_sort (function (a, b) -> function (c, d) -> b-d)
            (aux 0 arbol));;
*)