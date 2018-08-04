exception Invalid_move

type 'a play_outcome =
  { new_state: 'a ;
    switch_player: bool }

type game_outcome =
  | Draw
  | Winner_is of Player.t

type game_status =
  | Playing of Player.t
  | Game_over of game_outcome