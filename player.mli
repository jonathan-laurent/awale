open Core_kernel

type t = P0 | P1

val switch: t -> t
val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t
val compare : t -> t -> int

type rel = Self | Other

val relationship : t -> t -> rel