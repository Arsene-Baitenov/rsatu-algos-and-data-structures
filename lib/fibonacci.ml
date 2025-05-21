let fibs n =
  let fib_seq_gen = function
    | [] -> [ 1 ]
    | x :: [] -> [ x; x ]
    | x :: y :: fibs -> (x + y) :: x :: y :: fibs
  in
  let rec fib_seq = function
    | 0 -> fib_seq_gen []
    | n -> fib_seq_gen @@ fib_seq @@ (n - 1)
  in
  List.rev @@ fib_seq n
;;

let fib n =
  match List.rev @@ fibs n with
  | [] -> 0
  | f :: _ -> f
;;

type tree =
  | StubLeaf
  | Node of int * tree * tree
[@@deriving eq, show { with_path = false }]

let fib_tree k =
  let rec add t n =
    match t with
    | StubLeaf -> StubLeaf
    | Node (x, left, right) -> Node (x + n, add left n, add right n)
  in
  let rec helper = function
    | 0 -> StubLeaf, StubLeaf
    | 1 -> Node (fib 1, StubLeaf, StubLeaf), fst @@ helper 0
    | k ->
      let root = fib k in
      let prev, prev2 = helper @@ (k - 1) in
      let right = add prev2 root in
      Node (root, prev, right), prev
  in
  fst @@ helper k
;;

(*******************************Tests*******************************)

let%test "Fibonacci seq check" = fibs 6 = [ 1; 1; 2; 3; 5; 8; 13 ]
let%test "Fibonacci check 0" = fib 0 = 1
let%test "Fibonacci check 1" = fib 1 = 1
let%test "Fibonacci check 8" = fib 8 = 34
let print pp = Stdlib.Format.printf "%a" pp

let%expect_test "Fib tree k=0" =
  fib_tree 0 |> print pp_tree;
  [%expect {| StubLeaf |}]
;;

let%expect_test "Fib tree k=1" =
  fib_tree 1 |> print pp_tree;
  [%expect {| (Node (1, StubLeaf, StubLeaf)) |}]
;;

let%expect_test "Fib tree k=2" =
  fib_tree 2 |> print pp_tree;
  [%expect {| (Node (2, (Node (1, StubLeaf, StubLeaf)), StubLeaf)) |}]
;;

let%expect_test "Fib tree k=3" =
  fib_tree 3 |> print pp_tree;
  [%expect
    {|
    (Node (3, (Node (2, (Node (1, StubLeaf, StubLeaf)), StubLeaf)),
       (Node (4, StubLeaf, StubLeaf)))) |}]
;;

let%expect_test "Fib tree k=4" =
  fib_tree 4 |> print pp_tree;
  [%expect
    {|
    (Node (5,
       (Node (3, (Node (2, (Node (1, StubLeaf, StubLeaf)), StubLeaf)),
          (Node (4, StubLeaf, StubLeaf)))),
       (Node (7, (Node (6, StubLeaf, StubLeaf)), StubLeaf)))) |}]
;;

let%expect_test "Fib tree k=5" =
  fib_tree 5 |> print pp_tree;
  [%expect
    {|
    (Node (8,
       (Node (5,
          (Node (3, (Node (2, (Node (1, StubLeaf, StubLeaf)), StubLeaf)),
             (Node (4, StubLeaf, StubLeaf)))),
          (Node (7, (Node (6, StubLeaf, StubLeaf)), StubLeaf)))),
       (Node (11, (Node (10, (Node (9, StubLeaf, StubLeaf)), StubLeaf)),
          (Node (12, StubLeaf, StubLeaf))))
       )) |}]
;;
