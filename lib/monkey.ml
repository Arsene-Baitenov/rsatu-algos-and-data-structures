type 'a tree =
  | Node of 'a * 'a tree * 'a tree
  | Leaf of 'a
[@@deriving eq, show { with_path = false }]

let rec hight_of = function
  | Leaf _ -> 1
  | Node (_, left, right) -> max (hight_of left) (hight_of right)
;;

let full_left_restricted_tree k h =
  let rec helper k h min_p =
    match h, k with
    | 1, _ | _, 0 -> Leaf min_p, min_p
    | h, k ->
      let left, l_max_p = helper (k - 1) (h - 1) min_p in
      let right, r_max_p = helper k (h - 1) (l_max_p + 1) in
      Node (l_max_p + 1, left, right), r_max_p
  in
  fst @@ helper k h 0
;;

let strategy_lr_tree n k =
  let rec helper t min_p left_steps rem =
    match t with
    | Leaf _ ->
      if left_steps < k && rem >= 1
      then Node (min_p + 1, Leaf min_p, Leaf (min_p + 1)), min_p + 1, rem - 1
      else Leaf min_p, min_p, rem
    | Node (_, left, right) ->
      let left, l_max_p, rem = helper left min_p (left_steps + 1) rem in
      let right, r_max_p, rem = helper right (l_max_p + 1) left_steps rem in
      Node (l_max_p + 1, left, right), r_max_p, rem
  in
  let rec loop t = function
    | 0 -> t
    | rem ->
      let t, _, rem = helper t 0 0 rem in
      loop t rem
  in
  loop (Leaf 0) n
;;

let attempts_num n k = (hight_of @@ strategy_lr_tree n k) - 1

(*******************************Tests*******************************)

let print pp = Stdlib.Format.printf "%a" pp
let pp_itree = pp_tree Stdlib.Format.pp_print_int

let%expect_test "Full left-restricted tree k=2 h=5" =
  full_left_restricted_tree 2 5 |> print pp_itree;
  [%expect
    {|
    (Node (4,
       (Node (1, (Leaf 0), (Node (2, (Leaf 1), (Node (3, (Leaf 2), (Leaf 3))))))),
       (Node (7, (Node (5, (Leaf 4), (Node (6, (Leaf 5), (Leaf 6))))),
          (Node (9, (Node (8, (Leaf 7), (Leaf 8))),
             (Node (10, (Leaf 9), (Leaf 10)))))
          ))
       )) |}]
;;

let%expect_test "Full left-restricted tree k=1 h=4" =
  full_left_restricted_tree 1 4 |> print pp_itree;
  [%expect
    {|
    (Node (1, (Leaf 0), (Node (2, (Leaf 1), (Node (3, (Leaf 2), (Leaf 3))))))) |}]
;;

let%expect_test "Strategy left-restricted tree n=8 k=2" =
  strategy_lr_tree 8 2 |> print pp_itree;
  [%expect
    {|
    (Node (4,
       (Node (1, (Leaf 0), (Node (2, (Leaf 1), (Node (3, (Leaf 2), (Leaf 3))))))),
       (Node (7, (Node (5, (Leaf 4), (Node (6, (Leaf 5), (Leaf 6))))),
          (Node (8, (Leaf 7), (Leaf 8)))))
       )) |}]
;;

let%expect_test "Strategy left-restricted tree n=10 k=2" =
  strategy_lr_tree 10 2 |> print pp_itree;
  [%expect
    {|
    (Node (4,
       (Node (1, (Leaf 0), (Node (2, (Leaf 1), (Node (3, (Leaf 2), (Leaf 3))))))),
       (Node (7, (Node (5, (Leaf 4), (Node (6, (Leaf 5), (Leaf 6))))),
          (Node (9, (Node (8, (Leaf 7), (Leaf 8))),
             (Node (10, (Leaf 9), (Leaf 10)))))
          ))
       )) |}]
;;
