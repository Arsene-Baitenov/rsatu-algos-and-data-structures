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
    | Node of data * t * t
  [@@deriving show { with_path = false }, yojson]

  let empty = StubLeaf

  let find t k =
    let rec helper t k =
      match t with
      | StubLeaf -> None
      | Node ((node_k, node_v), left, right) ->
        if k = node_k
        then Some node_v
        else if k < node_k
        then helper left k
        else helper right k
    in
    helper t k
  ;;

  let insert t k v =
    let rec helper t k v =
      match t with
      | StubLeaf -> Node ((k, v), StubLeaf, StubLeaf)
      | Node ((node_k, node_v), left, right) ->
        if k = node_k
        then Node ((node_k, v), left, right)
        else if k < node_k
        then Node ((node_k, node_v), helper left k v, right)
        else Node ((node_k, node_v), left, helper right k v)
    in
    helper t k v
  ;;

  let remove t k =
    let rec insert_in_left t new_left =
      match t with
      | StubLeaf -> new_left
      | Node (d, left, right) -> Node (d, insert_in_left left new_left, right)
    in
    let rec helper t k =
      match t with
      | StubLeaf -> t
      | Node ((node_k, node_v), left, right) ->
        if k = node_k
        then insert_in_left right left
        else if k < node_k
        then Node ((node_k, node_v), helper left k, right)
        else Node ((node_k, node_v), left, helper right k)
    in
    helper t k
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
  TestTree.Node
    ( (16, "16")
    , TestTree.Node
        ( (9, "9")
        , TestTree.Node
            ( (8, "8")
            , TestTree.Node ((5, "5"), TestTree.StubLeaf, TestTree.StubLeaf)
            , TestTree.StubLeaf )
        , TestTree.Node
            ( (11, "11")
            , TestTree.Node ((10, "10"), TestTree.StubLeaf, TestTree.StubLeaf)
            , TestTree.Node ((12, "12"), TestTree.StubLeaf, TestTree.StubLeaf) ) )
    , TestTree.Node ((19, "19"), TestTree.StubLeaf, TestTree.StubLeaf) )
;;

let%expect_test "Insert 52 in tree" =
  TestTree.insert t 52 "52" |> print TestTree.pp;
  [%expect
    {|
    (Node ((16, "16"),
       (Node ((9, "9"),
          (Node ((8, "8"), (Node ((5, "5"), StubLeaf, StubLeaf)), StubLeaf)),
          (Node ((11, "11"), (Node ((10, "10"), StubLeaf, StubLeaf)),
             (Node ((12, "12"), StubLeaf, StubLeaf))))
          )),
       (Node ((19, "19"), StubLeaf, (Node ((52, "52"), StubLeaf, StubLeaf)))))) |}]
;;

let%expect_test "Insert 6 in tree" =
  TestTree.insert t 6 "6" |> print TestTree.pp;
  [%expect
    {|
    (Node ((16, "16"),
       (Node ((9, "9"),
          (Node ((8, "8"),
             (Node ((5, "5"), StubLeaf, (Node ((6, "6"), StubLeaf, StubLeaf)))),
             StubLeaf)),
          (Node ((11, "11"), (Node ((10, "10"), StubLeaf, StubLeaf)),
             (Node ((12, "12"), StubLeaf, StubLeaf))))
          )),
       (Node ((19, "19"), StubLeaf, StubLeaf)))) |}]
;;

let%expect_test "Insert 0 in tree" =
  TestTree.insert t 0 "0" |> print TestTree.pp;
  [%expect
    {|
    (Node ((16, "16"),
       (Node ((9, "9"),
          (Node ((8, "8"),
             (Node ((5, "5"), (Node ((0, "0"), StubLeaf, StubLeaf)), StubLeaf)),
             StubLeaf)),
          (Node ((11, "11"), (Node ((10, "10"), StubLeaf, StubLeaf)),
             (Node ((12, "12"), StubLeaf, StubLeaf))))
          )),
       (Node ((19, "19"), StubLeaf, StubLeaf)))) |}]
;;

let%expect_test "Find 0 in tree" =
  TestTree.find t 0 |> print V.pp_t_opt;
  [%expect {|
    None |}]
;;

let%expect_test "Find 12 in tree" =
  TestTree.find t 12 |> print V.pp_t_opt;
  [%expect {|
    (Some "12") |}]
;;

let%expect_test "Remove 12 from tree" =
  TestTree.remove t 12 |> print TestTree.pp;
  [%expect
    {|
    (Node ((16, "16"),
       (Node ((9, "9"),
          (Node ((8, "8"), (Node ((5, "5"), StubLeaf, StubLeaf)), StubLeaf)),
          (Node ((11, "11"), (Node ((10, "10"), StubLeaf, StubLeaf)), StubLeaf))
          )),
       (Node ((19, "19"), StubLeaf, StubLeaf)))) |}]
;;

let%expect_test "Remove 16 from tree" =
  TestTree.remove t 16 |> print TestTree.pp;
  [%expect
    {|
    (Node ((19, "19"),
       (Node ((9, "9"),
          (Node ((8, "8"), (Node ((5, "5"), StubLeaf, StubLeaf)), StubLeaf)),
          (Node ((11, "11"), (Node ((10, "10"), StubLeaf, StubLeaf)),
             (Node ((12, "12"), StubLeaf, StubLeaf))))
          )),
       StubLeaf)) |}]
;;

let%test "Json translation test" =
  let t2 = TestTree.of_json_string @@ TestTree.to_json_string t in
  Ok t = t2
;;
