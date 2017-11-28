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

(* [road] represents a road *)
type road = int * int

(* [port] represents a port, which offers a favorable exchange rate. *)
type port = {
  neighbors : int * int;
  demand : resource;
  rate : int
}
