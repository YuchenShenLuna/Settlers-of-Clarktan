open Elements
open Player

(* representation type for canvas background *)
type canvas = {
  tiles: Tile.tile list;
  ports: Trade.port list
}

(* state represents the game state type *)
type state = {
  robber: int;
  deck: DevCard.devcard list;
  players: Player.player list;
  canvas : canvas
}

(* [init_canvas] returns a new canvas to be used in a new game *)
val init_canvas : unit -> canvas

(* [fetch_tiles i] fetches the list of tiles associated with number [i] *)
val fetch_tiles : int -> Tile.tile list -> Tile.tile list

(* [play_devcard card st] returns the new state after the player plays [card]*)
val play_devcard : DevCard.devcard -> state -> state

(* [move_robber st] returns the new state after the player moves the
 * robber *)
val move_robber : state -> state

(* [build_building st] returns the new state after player builds a building *)
val build_building : state -> state

(* [build_road st] returns the new state after player builds a road *)
val build_road : state -> state

(* [trade_with_bank] returns the new state after player trades with bank *)
val trade_with_bank : state -> resource-> resource-> color -> player

(* [check_build_building num st col] returns true when player can build a building,
 * false otherwise *)
val check_build_settlements : int -> state -> color -> bool

(* [check_build_road road st col] returns true when player can build a road,
 * false otherwise *)
val check_build_road : road -> state -> color -> bool

(* [check_build_city num st col] checks whether a city can be build at the
 * index [num] for player with color [col] *)
val check_build_cities : int -> state -> color -> bool

(* [do_player st] returns the state after a player finishes a turn *)
val do_player : state -> state

(* [do_ai st] returns the state after an AI finishes a turn *)
val do_ai : state -> state
