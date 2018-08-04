open Game_common

module type GAME_STATE =
sig
  type t
  type move = int
  val valid_moves: t -> move list
  val play: t -> move -> t play_outcome
  val game_status: t -> game_status
  val utility: t -> float
end

module Make (GS: GAME_STATE):
sig
  exception No_possible_move
  type utilities_table = (GS.move * float) list
  val minmax: depth:int -> GS.t -> GS.move * utilities_table
end