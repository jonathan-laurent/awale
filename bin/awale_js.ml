
module AI = Minmax.Make(Board)

let current = ref (Board.initial)

let restart _ =
  current := Board.initial

let human_play move =
  current := (Board.play (!current) move).new_state

let ia_play depth =
  let move, _ = AI.minmax ~depth:depth (!current) in
  current := (Board.play (!current) move).new_state ;
  move

let _ =
  Js.export "awale_AI"
    (object%js
      method restart = restart ()
      method human_play x = human_play x
      method ia_play x = ia_play x
    end)
    