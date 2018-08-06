
module AI = Minmax.Make(Board)

let current = ref (Board.initial)

let restart _ =
  current := Board.initial

let play_move move =
  current := (Board.play (!current) move).new_state

let ia_play depth =
  let move, _ = AI.minmax ~depth:depth (!current) in
  move

let _ =
  Js.export "awale_AI"
    (object%js
      method restart = restart ()
      method play x = play_move x
      method ia x = ia_play x
    end)
    