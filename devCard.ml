type description = string

type devcard =
  | Knight
  | Road_of_Building
  | Year_of_Plenty
  | Monopoly
  | Victory_Point

(*cite from stackoverflow: how to shuffle elements of list in
  OCaml in O(n) time:
  https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml)*)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let input rs =
  failwith "TODO"

let rec remove_from_list dev lst =
  match lst with
  | [] -> []
  | h::t -> if h = dev then t else remove_from_list dev t
