open Core_kernel

type t = P0 | P1 [@@deriving sexp, compare]

let switch = function
  | P0 -> P1
  | P1 -> P0

type rel = Self | Other

let relationship p p' =
  if p = p' then Self else Other