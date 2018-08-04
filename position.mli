type t = private
  | Store of Player.t
  | House of Player.t * int

val store: Player.t -> t
val house: Player.t -> int -> t

exception Invalid

val first_house: int
val last_house: int
val num_houses_per_player: int

val house_indices: int list
val houses: Player.t -> t list

val next: t -> t
val next_skipping_store_of: player:Player.t -> t -> t