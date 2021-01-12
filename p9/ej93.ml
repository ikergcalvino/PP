open G_tree;;

let breadth_first_t arbol =
  let rec aux acc = function
      Gt (x, []) -> List.rev (x::acc)
    | Gt (x, Gt(raiz, ramas)::lista) ->
      aux (x::acc) (Gt(raiz, List.rev_append (List.rev lista) ramas))
    in aux [] arbol;;

let id x = x;;
let leaf v = Gt(v, []);;
let init_tree n = Gt(n, List.rev_map leaf (List.init n id));;
let t = init_tree 500_000;;