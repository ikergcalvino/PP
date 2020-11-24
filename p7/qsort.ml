let rec qsort1 ord = function
  [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
            qsort1 ord before @ h :: qsort1 ord after;;

let rec qsort2 ord =
  let append’ l1 l2 = List.rev_append (List.rev l1) l2 in
  function
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              append’ (qsort2 ord before) (h :: qsort2 ord after);;