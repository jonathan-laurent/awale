open Core_kernel
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

module Make (GS : GAME_STATE) =
struct

  exception No_possible_move

  let is_game_over st =
    match GS.game_status st with
      | Game_over _ -> true
      | Playing _ -> false

  let rec argmax = function
    | [] -> raise No_possible_move
    | [x, u] -> (x, u)
    | (x, u) :: l ->
      let x', u' = argmax l in
      if u > u' then x, u else x', u'

  let rec minmax_aux ~depth st =
    if depth = 0 || is_game_over st then
      GS.utility st, None
    else begin
      let options =
        List.map (GS.valid_moves st) ~f:(fun x ->
          let {new_state ; switch_player} = GS.play st x in
          let depth =
            if switch_player then depth-1 else depth in
          let utility, _ = minmax_aux ~depth new_state in
          let utility =
            if switch_player then -. utility else utility in
          x, utility
        ) in
      let best_move, best_utility = argmax options in
      best_utility, Some (best_move, options)
    end

  type utilities_table = (GS.move * float) list

  let minmax ~depth st =
    assert (depth > 0) ;
    match minmax_aux ~depth st with
      | _, None -> raise No_possible_move
      | _, Some (best_move, utilities) ->
        (best_move, utilities)
end