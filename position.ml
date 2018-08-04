open Core_kernel

type t =
  | Store of Player.t
  | House of Player.t * int

let first_house = 0

let last_house = 5

let num_houses_per_player = last_house - first_house + 1

exception Invalid

let store p = Store p

let house p x =
  if not (first_house <= x && x <= last_house) then raise Invalid ;
  House (p, x)

let next pos =
  match pos with
  | Store p -> House (Player.switch p, last_house)
  | House (p, x) ->
    if x > 0 then House (p, x-1)
    else Store p

let next_skipping_store_of ~player pos =
  match next pos with
    | Store p as pos' ->
      if p = player then next pos'
      else pos'
    | House _ as pos' -> pos'

let house_indices = List.range first_house (last_house+1)

let houses player =
  List.map house_indices ~f:(fun x -> house player x)