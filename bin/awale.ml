open Core_kernel
open In_channel
open Out_channel
open Scanf

module AI = Minmax.Make(Board)

let str_of_player = function
  | Player.P0 -> "AI"
  | Player.P1 -> "Human"

let minmax_depth = 6 (* 7 *)

let debug_utilities = true

let print_utilities fmt utilities =
  let open Format in
  utilities
  |> List.map ~f:(fun (x, u) -> sprintf "[%d]: %.0f" x u)
  |> String.concat ~sep:", "
  |> fprintf fmt "%s"

let main () =
  let rec loop awale =
    let player = Board.current_player awale in
    let rec prompt () =
      try
        printf "%s: " (str_of_player player) ;
        flush stdout ;
        sscanf (In_channel.input_line_exn stdin) "%d" (fun x ->
          if not (Board.is_valid_move awale x)
          then raise Game_common.Invalid_move ;
          x)
      with
        Scan_failure _ | Game_common.Invalid_move -> prompt ()
    in
    Format.printf "%a\n" Board.print awale ;
    flush stdout ;
    let move =
      begin match Board.current_player awale with
        | Player.P0 ->
          let move, utilities = AI.minmax ~depth:minmax_depth awale in
          if debug_utilities then
          begin
            Format.printf "Utilities: %a.\n" print_utilities utilities
          end ;
          printf "The AI plays %d.\n" move ;
          move
        | Player.P1 ->
          prompt ()
      end in
    let awale = (Board.play awale move).new_state in
      match Board.game_status awale with
      | Playing _ -> loop awale
      | Game_over outcome ->
        begin match outcome with
          | Draw -> printf "The game ends with a draw."
          | Winner_is winner ->
            printf "The winner is: %s!" (str_of_player winner)
        end ;
        Format.printf "%a\n" Board.print awale
  in
  loop (Board.initial)


let () =
  try main ()
  with End_of_file -> ()