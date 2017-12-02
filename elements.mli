(* [color] represents a color, which identifies a player. *)
type color =
  | Red
  | Yellow
  | Blue
  | Green

(* [resource] represents a resource. *)
type resource =
  | Lumber
  | Wool
  | Grain
  | Brick
  | Ore

(* [card] represents a development card. *)
type card =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

(* [intersection] represents an intersection.*)
type intersection = int

(* [edge] represents an edge *)
type edge = int * int

(* [shuffle lst] is a permutation of [lst]. *)
val shuffle : 'a list -> 'a list
