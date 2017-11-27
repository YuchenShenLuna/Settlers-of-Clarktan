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
  turn : color;
  players: Player.player list;
  canvas : canvas
}

val longest_road : state -> player option

val largest_army : state -> player option

val end_turn : state -> state

val init_state : unit -> state

(* [init_canvas] returns a new canvas to be used in a new game *)
val init_canvas : unit -> canvas

(* [fetch_tiles i] fetches the list of tiles associated with number [i] *)
val fetch_tiles : int -> Tile.tile list -> Tile.tile list

(* [init_build_settlement ind color st] changes the state when player
 * with color [color] at state [st] builds a settlement at index [ind] *)
val init_build_settlement : int -> color -> state -> state

(* [init_build_road road color st] changes the state when player
 * with color [color] at state [st] builds a road at index [road] *)
val init_build_road : road -> color -> state -> state

(* [init_generate_resources color st] changes the state when in the initial
 * phase the player with color [color] gets the auto-generated resources
 * after building initial settlements and roads at state [st] *)
val init_generate_resources : color -> state -> state

(* [set_up] initializes the first stage of the game *)
val setup : state -> state

(* [discard_resource] discards resources for one player when robber is activated *)
val discard_resource : color -> state -> (resource*int) list -> state

(* [play_robber st] returns the new state after the player plays the
 * robber by dice 7 *)
val play_robber : state -> color -> int -> state

(* [build_building st] returns the new state after player builds a building *)
val build_settlement : int -> state -> color -> state

(* [build_road st] returns the new state after player builds a road *)
val build_road : road -> state -> color -> state

(* [build_city st] returns the new state after player builds a city *)
val build_city : int -> state -> color -> state

(* [trade_with_bank] returns the new state after player trades with bank *)
val trade_with_bank : state -> (resource * int) list -> (resource * int) list -> color -> state

val trade_with_player : state -> (resource * int) list -> (resource * int) list -> color -> state

(* [check_build_building num st col] returns true when player can build a building,
 * false otherwise *)
val check_build_settlements : int -> state -> color -> bool

(* [check_build_road road st col] returns true when player can build a road,
 * false otherwise *)
val check_build_road : road -> state -> color -> bool

(* [check_build_city num st col] checks whether a city can be build at the
 * index [num] for player with color [col] *)
val check_build_cities : int -> state -> color -> bool

(* [generate_resource st] generates the resource for each dice roll *)
val generate_resource : state -> int -> state

(* [do_player st] returns the state after a player finishes a turn *)
val do_player : state -> state

(* [do_ai st] returns the state after an AI finishes a turn *)
val do_ai : state -> state
