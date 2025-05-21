type tree =
  | StubLeaf
  | Node of int * tree * tree
[@@deriving eq, ord, show { with_path = false }]

let build_ideal_balance_tree n =
  let rec helper min max =
    if max < min
    then StubLeaf
    else (
      let ln = (max + 1 - min) / 2 in
      let root_number = min + ln in
      let left = helper min (root_number - 1) in
      let right = helper (root_number + 1) max in
      Node (root_number, left, right))
  in
  helper 1 n
;;

(*******************************Tests*******************************)

let print pp = Stdlib.Format.printf "%a" pp

let%expect_test "Binary tree with ideal balance (n = 4)" =
  build_ideal_balance_tree 4 |> print pp_tree;
  [%expect
    {|
  (Node (3, (Node (2, (Node (1, StubLeaf, StubLeaf)), StubLeaf)),
     (Node (4, StubLeaf, StubLeaf)))) |}]
;;

let%expect_test "Binary tree with ideal balance (n = 7)" =
  build_ideal_balance_tree 7 |> print pp_tree;
  [%expect
    {|
  (Node (4,
     (Node (2, (Node (1, StubLeaf, StubLeaf)), (Node (3, StubLeaf, StubLeaf)))),
     (Node (6, (Node (5, StubLeaf, StubLeaf)), (Node (7, StubLeaf, StubLeaf))))
     )) |}]
;;
