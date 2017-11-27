type color = Red | Yellow | Blue | Green | White

type resource = Lumber | Wool | Grain | Brick | Ore

type road = int * int

type port = {
  neighbors : int * int;
  resource : resource;
  rate : int
}
