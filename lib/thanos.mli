type tree =
  | StubLeaf
  | Node of int * tree * tree

val equal_tree : tree -> tree -> bool
val compare_tree : tree -> tree -> int
val pp_tree : Format.formatter -> tree -> unit
val show_tree : tree -> string
val build_ideal_balance_tree : int -> tree
