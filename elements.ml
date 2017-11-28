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

type road = int * int

type port = {
  neighbors : int * int;
  demand : resource;
  rate : int
}
