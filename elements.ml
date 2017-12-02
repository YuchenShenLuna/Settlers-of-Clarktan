type color =
  | Red
  | Yellow
  | Blue
  | Green

type resource =
  | Lumber
  | Wool
  | Grain
  | Brick
  | Ore

type card =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

type intersection = int

type edge = int * int

(* [shuffle lst] is a permutation of [lst].
 * Citation: The following code snippet is from Stack Overflow
 * Link: https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml *)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond
