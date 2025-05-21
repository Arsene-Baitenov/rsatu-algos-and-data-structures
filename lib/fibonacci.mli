val fibs : int -> int list
val fib : int -> int

type tree =
  | StubLeaf
  | Node of int * tree * tree

val equal_tree : tree -> tree -> bool
val pp_tree : Format.formatter -> tree -> unit
val show_tree : tree -> string
val fib_tree : int -> tree
