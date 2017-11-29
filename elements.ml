type color =
  | Red
  | Yellow
  | Blue
  | Green
  | White

type resource =
  | Lumber
  | Wool
  | Grain
  | Brick
  | Ore
  | Null

(* [card] represents a development card. *)
type card =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

type intersection = int

type edge = int * int

(* Citation: The following code snippet is from Stack Overflow
 * Topic: "How to shuffle list in O(n) in OCaml?"
 * Link: https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml *)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond
