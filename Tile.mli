(* Tile module keeps information about the tiles in the game *)

(* resource is the type of resources for tiles in the game *)
type resource = Lumber | Wool | Grain | Brick | Ore

(* tile is the type of tiles that hold information of tiles in the game *)
type tile = {
  dice : int;
  resource : resource;
  center : float * float;
  edge : float
}

(* [neighbors tile] returns the list of coordinates boardering [tile] *)
val neighbors : tile -> (int * int) list
