(* Tile module keeps information about the tiles in the game *)

(* resource is the type of resources for tiles in the game *)
type resource

(* coordinate is the type of coordinates in the game *)
type coordinate

(* tile is the type of tiles that hold information of tiles in the game *)
type tile

(* [update_tile_list tile lst] updates the list of tiles in the game *)
val update_tile_list : tile -> tile list -> tile list

(* [fetch_robber] returns the tile with robber in the game *)
val fetch_robber : tile list -> tile

(* [update_robber lst] updates the list of tiles with robber moved *)
val update_robber : tile -> tile list -> tile list

(* [init_tiles] initializes the tiles for a game play *)
val init_tiles : unit -> tile list

(* [neighbors tile] returns the list of coordinates boardering [tile] *)
val neighbors : tile -> coordinate list
