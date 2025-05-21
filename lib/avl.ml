module Tree
    (K : sig
       type t [@@deriving eq, ord, show { with_path = false }, yojson]

       [@@@end]
     end)
    (V : sig
       type t [@@deriving show { with_path = false }, yojson]

       [@@@end]
     end) =
struct
  type data = K.t * V.t [@@deriving show { with_path = false }, yojson]

  type t =
    | StubLeaf
    | Node of data * int * t * t
  [@@deriving show { with_path = false }, yojson]

  let empty = StubLeaf

  let find t k =
    let rec helper t k =
      match t with
      | StubLeaf -> None
      | Node ((node_k, node_v), _, left, right) ->
        if k = node_k
        then Some node_v
        else if k < node_k
        then helper left k
        else helper right k
    in
    helper t k
  ;;

  let balance_factor = function
    | Node (_, b, _, _) -> b
    | StubLeaf -> 0
  ;;

  let balance = function
    | Node (ad, -2, p, Node (bd, bbf, q, r)) when bbf = 0 || bbf = -1 ->
      Node (bd, -bbf - 1, Node (ad, bbf + 1, p, q), r)
    | Node (bd, 2, Node (ad, abf, p, q), r) when abf = 0 || abf = 1 ->
      Node (ad, abf - 1, p, Node (bd, abf - 1, q, r))
    | Node (ad, -2, p, Node (bd, _, Node (cd, cbf, q, r), s)) ->
      Node (cd, 0, Node (ad, (1 - cbf) / 2, p, q), Node (bd, -((1 + cbf) / 2), r, s))
    | Node (bd, 2, Node (ad, _, p, Node (cd, cbf, q, r)), s) ->
      Node (cd, 0, Node (ad, -((1 + cbf) / 2), p, q), Node (bd, (1 - cbf) / 2, r, s))
    | t -> t
  ;;

  let insert t k v =
    let rec helper t k v =
      match t with
      | StubLeaf -> Node ((k, v), 0, StubLeaf, StubLeaf), true
      | Node ((node_k, node_v), bf, left, right) ->
        if k = node_k
        then Node ((node_k, v), bf, left, right), false
        else (
          let left, right, bf =
            if k < node_k
            then (
              let left, grew = helper left k v in
              let bf = bf + if grew then 1 else 0 in
              left, right, bf)
            else (
              let right, grew = helper right k v in
              let bf = bf - if grew then 1 else 0 in
              left, right, bf)
          in
          let balanced = balance @@ Node ((node_k, node_v), bf, left, right) in
          let grew = balance_factor balanced <> 0 in
          balanced, grew)
    in
    fst @@ helper t k v
  ;;

  let remove t k =
    let rec leftmost_data_opt = function
      | StubLeaf -> None
      | Node (d, _, StubLeaf, _) -> Some d
      | Node (_, _, l, _) -> leftmost_data_opt l
    in
    let rec helper t k =
      match t with
      | StubLeaf -> t, false
      | Node ((node_k, node_v), bf, left, right) ->
        let to_balance =
          if k = node_k
          then (
            match leftmost_data_opt right with
            | Some (lm_k, lm_v) ->
              let right, shrunk = helper right lm_k in
              let bf = bf + if shrunk then 1 else 0 in
              Node ((lm_k, lm_v), bf, left, right)
            | None -> left)
          else if k < node_k
          then (
            let left, shrunk = helper left k in
            let bf = bf - if shrunk then 1 else 0 in
            Node ((node_k, node_v), bf, left, right))
          else (
            let right, shrunk = helper right k in
            let bf = bf + if shrunk then 1 else 0 in
            Node ((node_k, node_v), bf, left, right))
        in
        let balanced = balance to_balance in
        let shrunk = balance_factor balanced = 0 in
        balanced, shrunk
    in
    fst @@ helper t k
  ;;

  let to_json_string t =
    let json = to_yojson t in
    Yojson.Safe.to_string json
  ;;

  let of_json_string s =
    let json = Yojson.Safe.from_string s in
    of_yojson json
  ;;
end

(*******************************Tests*******************************)

module K = struct
  type t = int [@@deriving eq, ord, show { with_path = false }, yojson]
end

module V = struct
  type t = string [@@deriving show { with_path = false }, yojson]
  type t_opt = t option [@@deriving show { with_path = false }]
end

module TestTree = Tree (K) (V)

let print pp = Stdlib.Format.printf "%a" pp

let t =
  let rec helper t n =
    match n with
    | 0 -> t
    | _ ->
      let i = Random.int 20 in
      let t = TestTree.insert t i (Int.to_string i) in
      helper t (n - 1)
  in
  helper TestTree.empty 10
;;

let%expect_test "Insert 52 in tree" =
  TestTree.insert t 52 "52" |> print TestTree.pp;
  [%expect
    {|
    (Node ((8, "8"), 0,
       (Node ((4, "4"), -1, (Node ((2, "2"), 0, StubLeaf, StubLeaf)),
          (Node ((6, "6"), -1, StubLeaf, (Node ((7, "7"), 0, StubLeaf, StubLeaf))
             ))
          )),
       (Node ((13, "13"), 0,
          (Node ((9, "9"), -1, StubLeaf,
             (Node ((11, "11"), 0, StubLeaf, StubLeaf)))),
          (Node ((15, "15"), -1, StubLeaf,
             (Node ((52, "52"), 0, StubLeaf, StubLeaf))))
          ))
       )) |}]
;;

let%expect_test "Insert 5 in tree" =
  TestTree.insert t 5 "5" |> print TestTree.pp;
  [%expect
    {|
    (Node ((8, "8"), 1,
       (Node ((4, "4"), -1, (Node ((2, "2"), 0, StubLeaf, StubLeaf)),
          (Node ((6, "6"), 0, (Node ((5, "5"), 0, StubLeaf, StubLeaf)),
             (Node ((7, "7"), 0, StubLeaf, StubLeaf))))
          )),
       (Node ((13, "13"), 1,
          (Node ((9, "9"), -1, StubLeaf,
             (Node ((11, "11"), 0, StubLeaf, StubLeaf)))),
          (Node ((15, "15"), 0, StubLeaf, StubLeaf))))
       )) |}]
;;

let%expect_test "Insert 0 in tree" =
  TestTree.insert t 0 "0" |> print TestTree.pp;
  [%expect
    {|
    (Node ((8, "8"), 0,
       (Node ((4, "4"), 0,
          (Node ((2, "2"), 1, (Node ((0, "0"), 0, StubLeaf, StubLeaf)), StubLeaf
             )),
          (Node ((6, "6"), -1, StubLeaf, (Node ((7, "7"), 0, StubLeaf, StubLeaf))
             ))
          )),
       (Node ((13, "13"), 1,
          (Node ((9, "9"), -1, StubLeaf,
             (Node ((11, "11"), 0, StubLeaf, StubLeaf)))),
          (Node ((15, "15"), 0, StubLeaf, StubLeaf))))
       )) |}]
;;

let%expect_test "Find 0 in tree" =
  TestTree.find t 0 |> print V.pp_t_opt;
  [%expect {|
    None |}]
;;

let%expect_test "Find 6 in tree" =
  TestTree.find t 6 |> print V.pp_t_opt;
  [%expect {|
    (Some "6") |}]
;;

let%expect_test "Remove 4 from tree" =
  TestTree.remove t 4 |> print TestTree.pp;
  [%expect
    {|
    (Node ((8, "8"), -1,
       (Node ((6, "6"), 0, (Node ((2, "2"), 0, StubLeaf, StubLeaf)),
          (Node ((7, "7"), 0, StubLeaf, StubLeaf)))),
       (Node ((13, "13"), 1,
          (Node ((9, "9"), -1, StubLeaf,
             (Node ((11, "11"), 0, StubLeaf, StubLeaf)))),
          (Node ((15, "15"), 0, StubLeaf, StubLeaf))))
       )) |}]
;;

let%expect_test "Remove 8 from tree" =
  TestTree.remove t 8 |> print TestTree.pp;
  [%expect
    {|
    (Node ((9, "9"), 1,
       (Node ((4, "4"), -1, (Node ((2, "2"), 0, StubLeaf, StubLeaf)),
          (Node ((6, "6"), -1, StubLeaf, (Node ((7, "7"), 0, StubLeaf, StubLeaf))
             ))
          )),
       (Node ((13, "13"), 0, (Node ((11, "11"), 0, StubLeaf, StubLeaf)),
          (Node ((15, "15"), 0, StubLeaf, StubLeaf))))
       )) |}]
;;

let%test "Json translation test" =
  let t2 = TestTree.of_json_string @@ TestTree.to_json_string t in
  Ok t = t2
;;
