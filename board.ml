open Core_kernel
open Game_common

let initial_nseeds_per_house = 3
let total_number_of_seeds =
  2 * Position.num_houses_per_player * initial_nseeds_per_house

module type UNSAFE_IMPL =
sig
  type t
  val init: Player.t -> t
  val copy: t -> t
  val update: t -> Position.t -> f:(int -> int) -> unit
  val get: t -> Position.t -> int
  val sum_houses: t -> Player.t -> int
  val clear_houses: t -> Player.t -> unit
  val set_game_over: t -> bool -> unit
  val is_game_over: t -> bool
  val switch_current_player: t -> unit
  val get_current_player: t -> Player.t
  val symmetrize: t -> t
  val t_of_sexp: Sexp.t -> t
  val sexp_of_t: t -> Sexp.t
end

module Unsafe : UNSAFE_IMPL =
struct

  type t =
    { data: int array ;
      mutable game_over: bool ;
      mutable cur_player: Player.t }
    [@@deriving sexp]

  let n = Position.num_houses_per_player
  let datalen = n + 1
  let store_idx = n

  let init cur_player =
    let data = Array.create ~len:(2*datalen) initial_nseeds_per_house in
    data.(store_idx) <- 0 ;
    data.(store_idx + datalen) <- 0 ;
    { data ; cur_player ; game_over = false }

  let copy b = { b with data = Array.copy b.data }

  let idx_of_player = function
    | Player.P0 -> 0
    | Player.P1 -> datalen

  let idx_of_pos = function
    | Position.Store p -> idx_of_player p + store_idx
    | Position.House (p, x) -> idx_of_player p + x

  let update b pos ~f =
    let i = idx_of_pos pos in
    b.data.(i) <- f b.data.(i)

  let get b pos = b.data.(idx_of_pos pos)

  let sum_houses b p =
    let start = idx_of_player p in
    let s = ref 0 in
    for i = start to start + n - 1 do
      s := !s + b.data.(i)
    done ;
    !s

  let clear_houses b p =
    let start = idx_of_player p in
    for i = start to start + n - 1 do
      b.data.(i) <- 0
    done

  let set_game_over b v = b.game_over <- v

  let is_game_over b = b.game_over

  let switch_current_player b =
    b.cur_player <- Player.switch b.cur_player

  let get_current_player b = b.cur_player

  let symmetrize b =
    match get_current_player b with
    | P0 -> b
    | P1 ->
      let data = Array.init (Array.length b.data) ~f:(fun i ->
        if i < datalen
        then b.data.(i + datalen)
        else b.data.(i - datalen)
      ) in
      { data ; game_over = b.game_over ; cur_player = Player.P0 }

end


module Asymmetric =
struct

  type t = Unsafe.t [@@deriving sexp]

  type move = int

  let initial = Unsafe.init Player.P0

  let nseeds = Unsafe.get

  let print f b =
    let pr = Format.fprintf f "%s" in
    let store_content player =
      Format.sprintf " ( %2d ) " (nseeds b (Position.store player)) in
    let store_0, store_1 = store_content Player.P0, store_content Player.P1 in
    let repeat_pr num str =
      for _ = 1 to num do pr str done in
    let houses player =
      let positions = Position.houses player in
      let positions =
        if player = Player.P1 then List.rev positions else positions in
      List.map positions ~f:(fun pos -> Format.sprintf "%2d" (nseeds b pos))
      |> String.concat ~sep:" | " in
    let houses_0, houses_1 = houses Player.P0, houses Player.P1 in
    pr "\n" ;
    repeat_pr (String.length store_0) " " ;
    pr houses_0 ;
    pr "\n" ;
    pr store_0 ;
    repeat_pr (String.length houses_0) "=" ;
    pr store_1 ;
    pr "\n" ;
    repeat_pr (String.length store_0) " " ;
    pr houses_1 ;
    pr "\n"

  let check_game_over b =
    let p = Unsafe.get_current_player b in
    if Unsafe.sum_houses b p = 0 then
    begin
      let other_p = Player.switch p in
      let remaining = Unsafe.sum_houses b other_p in
      Unsafe.set_game_over b true ;
      Unsafe.clear_houses b other_p ;
      Unsafe.update b (Position.store p) ~f:(fun x -> x + remaining)
    end

  let is_valid_move b x =
    try
      let player = Unsafe.get_current_player b in
      let pos = Position.house player x in
      Unsafe.get b pos > 0 && not (Unsafe.is_game_over b)
    with Position.Invalid -> false

  let valid_moves b =
    List.filter Position.house_indices ~f:(is_valid_move b)

  let play b x =
    let b = Unsafe.copy b in
    if not (is_valid_move b x) then raise Invalid_move ;
    let player = Unsafe.get_current_player b in
    let rec sow_from nseeds pos =
      assert (nseeds >= 0) ;
      if nseeds = 0 then pos
      else begin
        let pos =
          Position.next_skipping_store_of ~player:(Player.switch player) pos in
        Unsafe.update b pos ~f:(fun x -> x + 1) ;
        sow_from (nseeds-1) pos
      end
    in
    let pos = Position.house player x in
    let nseeds = Unsafe.get b pos in
    Unsafe.update b pos ~f:(fun _ -> 0) ;
    let final_pos = sow_from nseeds pos in
    let switch_player =
      match final_pos with
        | Store _ -> false
        | House _ -> Unsafe.switch_current_player b ; true in
    check_game_over b ;
    { new_state = b ; switch_player }

  let game_status b =
    if Unsafe.is_game_over b then
      let delta =
        nseeds b (Position.store Player.P0) -
        nseeds b (Position.store Player.P1) in
      let outcome =
        if delta = 0 then Draw
        else if delta > 0 then Winner_is Player.P0
        else Winner_is Player.P1 in
      Game_over outcome
    else Playing (Unsafe.get_current_player b)


  let current_player = Unsafe.get_current_player

  let seeds_majority = total_number_of_seeds / 2 + 1

  let utility b =
    let player = current_player b in
    let other_player = Player.switch player in
    let player_score = nseeds b (Position.store player) in
    let other_player_score = nseeds b (Position.store other_player) in
    if player_score >= seeds_majority then Float.infinity
    else if other_player_score >= seeds_majority then -. Float.infinity
    else float_of_int (player_score - other_player_score)

end

include Asymmetric

module Symmetric =
struct
  module Self =
  struct
    type t = Asymmetric.t [@@deriving sexp]

    let hash b = Hashtbl.hash (Unsafe.symmetrize b)

    let compare b b' =
      compare (Unsafe.symmetrize b) (Unsafe.symmetrize b')
  end

  include Self
  module Table = Hashtbl.Make (Self)

end

let symmetric_view t = t