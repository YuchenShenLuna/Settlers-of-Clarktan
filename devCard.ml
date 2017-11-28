type devcard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

(* Citation: The following code snippet is from Stack Overflow
 * Topic: "How to shuffle list in O(n) in OCaml?"
 * Link: https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml
 *)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond
