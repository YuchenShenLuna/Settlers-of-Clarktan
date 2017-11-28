(* [color] represents a color, which identifies a player. *)
type color =
  | Red
  | Yellow
  | Blue
  | Green
  | White

(* [resource] represents a resource. *)
type resource =
  | Lumber
  | Wool
  | Grain
  | Brick
  | Ore
  | Null

(* [intersection] represents an intersection.*)
type intersection = int

(* [edge] represents an edge *)
type edge = int * int
