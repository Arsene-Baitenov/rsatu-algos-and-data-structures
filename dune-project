(lang dune 3.16)

(name algos)

(generate_opam_files true)

(source
 (github Arsene-Baitenov/rsatu-algos-and-data-structures))

(authors "Arsene Baitenov")

(maintainers "Arsene Baitenov")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name algos)
 (synopsis "Programs for course Algos and Data structures")
 (description
  "Programming materials for the RSATU course Algorithms and data structures")
 (depends ocaml dune ppx_expect ppx_deriving)
 (tags
  ("binary tree" "dp" "avl" "Fibonacci tree" "left-restricted tree")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
