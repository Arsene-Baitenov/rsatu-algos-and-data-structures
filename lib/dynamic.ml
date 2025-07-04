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

(*******************************With memo*******************************)

let memo_rec open_rec_f =
  let h = Base.Hashtbl.Poly.create () in
  let rec f arg_tuple =
    match Base.Hashtbl.Poly.find h arg_tuple with
    | Some res -> res
    | None ->
      let res = open_rec_f f arg_tuple in
      Base.Hashtbl.Poly.set h ~key:arg_tuple ~data:res;
      res
  in
  f
;;

(*******************************Common subseq with max length*******************************)

let largest_common_subseq xs ys =
  let helper self (xs, ys) =
    match xs, ys with
    | [], _ | _, [] -> []
    | x :: xs, y :: ys ->
      let res1 = self (x :: xs, ys) in
      let res2 = self (xs, y :: ys) in
      let res = if List.length res1 > List.length res2 then res1 else res2 in
      let res3 = if x = y then x :: self (xs, ys) else [] in
      if List.length res > List.length res3 then res else res3
  in
  memo_rec helper (xs, ys)
;;

(*******************************Optimal matrix multiplication*******************************)

let optimal_matrix_multiplication ms =
  let ms = Array.of_list ms in
  let n = Array.length ms in
  let helper self (l, r) =
    if l = r
    then 0, []
    else (
      let h_l = fst ms.(l) in
      let w_r = snd ms.(r) in
      let rec loop i (optimal, mult_ord) =
        if i >= r
        then optimal, mult_ord
        else (
          let w_i = snd ms.(i) in
          let l_c, l_mo = self (l, i) in
          let r_c, r_mo = self (i + 1, r) in
          let c = l_c + r_c + (h_l * w_i * w_r) in
          let mo = l_mo @ r_mo @ [ i ] in
          (* let cost = self l i + self (i + 1) r + (h_l * w_i * w_r) in *)
          let acc = if c < optimal then c, mo else optimal, mult_ord in
          loop (i + 1) acc)
      in
      loop l (max_int, []))
  in
  memo_rec helper (0, n - 1)
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

(*******************************Tests Common subseq with max length*******************************)

let%expect_test "Check int 1" =
  largest_common_subseq [ 1; 2; 3; 4; 5 ] [ 2; 7; 3; 2; 5 ] |> print pp_ilist;
  [%expect {| [2; 3; 5] |}]
;;

let%expect_test "Check int 2" =
  largest_common_subseq [ 7; 2; 9; 3; 1; 5; 9; 4 ] [ 2; 8; 1; 3; 9; 7 ] |> print pp_ilist;
  [%expect {| [2; 1; 9] |}]
;;

let%expect_test "Check char 1" =
  largest_common_subseq [ 'A'; 'B'; 'C'; 'D'; 'B'; 'A'; 'B' ] [ 'B'; 'C'; 'D'; 'B' ]
  |> print pp_clist;
  [%expect {| ['B'; 'C'; 'D'; 'B'] |}]
;;

let%expect_test "Check char 2" =
  largest_common_subseq [ 'A'; 'C'; 'E' ] [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F' ]
  |> print pp_clist;
  [%expect {| ['A'; 'C'; 'E'] |}]
;;

let%expect_test "Check char 3" =
  largest_common_subseq
    [ 'a'; 'b'; 'c'; 'a'; 'b'; 'a'; 'a'; 'c' ]
    [ 'b'; 'a'; 'c'; 'c'; 'b'; 'c'; 'a' ]
  |> print pp_clist;
  [%expect {| ['b'; 'a'; 'b'; 'c'] |}]
;;

(*******************************Tests Optimal matrix multiplication*******************************)

type ans_helper = int * int list [@@deriving show { with_path = false }]

let%expect_test "Check 1" =
  optimal_matrix_multiplication [ 10, 30; 30, 5; 5, 60 ] |> print pp_ans_helper;
  [%expect {| (4500, [0; 1]) |}]
;;

let%expect_test "Check 2" =
  optimal_matrix_multiplication [ 1, 5; 5, 20; 20, 1 ] |> print pp_ans_helper;
  [%expect {| (105, [1; 0]) |}]
;;

let%expect_test "Check 3" =
  optimal_matrix_multiplication [ 5, 10; 10, 20; 20, 35 ] |> print pp_ans_helper;
  [%expect {| (4500, [0; 1]) |}]
;;

let%expect_test "Check 4" =
  optimal_matrix_multiplication [ 30, 35; 35, 15; 15, 5; 5, 10; 10, 20; 20, 25 ]
  |> print pp_ans_helper;
  [%expect {| (15125, [1; 0; 3; 4; 2]) |}]
;;
