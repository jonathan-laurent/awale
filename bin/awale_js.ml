
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
