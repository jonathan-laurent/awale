open Core_kernel
open Game_common

type t

type move = int

val initial: t

val is_valid_move: t -> move -> bool

val valid_moves: t -> move list

val play: t -> move -> t play_outcome

val nseeds: t -> Position.t -> int

val game_status: t -> game_status

val current_player: t -> Player.t

val print: Format.formatter -> t -> unit

val utility: t -> float


(* Symmetric view for MCTS *)

module Symmetric:
sig
  type t
  module Table: Hashtbl.S
end

val symmetric_view: t -> Symmetric.t
