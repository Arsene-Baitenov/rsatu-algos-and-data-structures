(*******************************Increasing (or by cmp) subseq of max length*******************************)

let max_subseq_by_cmp_f xs cmp_f =
  let rec traversal_cps x xlrs k =
    match xlrs with
    | [] -> k (x, 1, [ x ])
    | (prev_x, l, subseq) :: tail ->
      traversal_cps x tail (fun (_, ol, osubseq) ->
        k
          (if cmp_f x prev_x
           then if l + 1 >= ol then x, l + 1, x :: subseq else x, ol, osubseq
           else x, ol, osubseq))
  in
  let traversal x xlrs = traversal_cps x xlrs Fun.id in
  let rec helper_cps xs k =
    match xs with
    | [] -> k []
    | x :: xs -> helper_cps xs (fun xlrs -> k (traversal x xlrs :: xlrs))
  in
  let helper xs = helper_cps xs Fun.id in
  match helper @@ List.rev xs with
  | [] -> []
  | (_, _, subseq) :: _ -> List.rev subseq
;;

(*******************************Tests*******************************)

type ilist = int list [@@deriving show { with_path = false }]
type slist = string list [@@deriving show { with_path = false }]
type clist = char list [@@deriving show { with_path = false }]

let print pp = Stdlib.Format.printf "%a" pp

(*******************************Tests Increasing (or by cmp) subseq of max length*******************************)

let%expect_test "Check int >= " =
  max_subseq_by_cmp_f [ 7; 6; 5; 4; 3; 2; 1 ] ( >= ) |> print pp_ilist;
  [%expect {| [1] |}]
;;

let%expect_test "Check int < " =
  max_subseq_by_cmp_f [ 7; 6; 5; 4; 3; 2; 1 ] ( < ) |> print pp_ilist;
  [%expect {| [7; 6; 5; 4; 3; 2; 1] |}]
;;

let%expect_test "Check string >= " =
  let str_ge s1 s2 = String.compare s1 s2 >= 0 in
  max_subseq_by_cmp_f [ "a"; "b"; "c"; "d"; "d"; "e"; "f" ] str_ge |> print pp_slist;
  [%expect {| ["a"; "b"; "c"; "d"; "d"; "e"; "f"] |}]
;;

let%expect_test "Check string >= 2" =
  let str_ge s1 s2 = String.compare s1 s2 >= 0 in
  max_subseq_by_cmp_f [ "f"; "e"; "d"; "c"; "b"; "a" ] str_ge |> print pp_slist;
  [%expect {| ["a"] |}]
;;

let%expect_test "Check string <= " =
  (* let str_le s1 s2 = String.compare s1 s2 <= 0 in *)
  max_subseq_by_cmp_f [ "f"; "e"; "d"; "c"; "d"; "b"; "a" ] (fun s1 s2 ->
    String.compare s1 s2 <= 0)
  |> print pp_slist;
  [%expect {| ["f"; "e"; "d"; "d"; "b"; "a"] |}]
;;
