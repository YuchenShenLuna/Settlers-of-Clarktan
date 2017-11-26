type description = string

type devcard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

(*cite from stackoverflow: how to shuffle elements of list in
  OCaml in O(n) time:
  https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml)*)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond
