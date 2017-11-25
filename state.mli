(* representation type for canvas background *)
type canvas = {
  tiles: Tile.tile list;
  ports: Trade.port list
}

(* state represents the game state type *)
type state = {
  robber: int;
  deck: DevCard.devcard list;
  human: Player.player;
  zikiu: Player.player;
  iris: Player.player;
  mike: Player.player;
  canvas : canvas
}

(* [init_canvas] returns a new canvas to be used in a new game *)
val init_canvas : unit -> canvas

(* [fetch_tiles i] fetches the list of tiles associated with number [i] *)
val fetch_tiles : int -> Tile.tile list

(* [play_devcard card st] returns the new state after the player plays [card]*)
val play_devcard : DevCard.devcard -> state -> state

(* [move_robber st] returns the new state after the player moves the
 * robber *)
val move_robber : state -> state

(* [build_building st] returns the new state after player builds a building *)
val build_building : state -> state

(* [build_road st] returns the new state after player builds a road *)
val build_road : state -> state

(* [trade st] returns the new state after player trades *)
val trade : state -> state

(* [check_build_building coor st] returns true when player can build a building,
 * false otherwise *)
val check_build_building : Tile.tile -> state -> bool

(* [check_build_road coord st] returns true when player can build a road,
 * false otherwise *)
val check_build_road : Tile.tile -> state -> bool

(* [do_player st] returns the state after a player finishes a turn *)
val do_player : state -> state

(* [do_ai st] returns the state after an AI finishes a turn *)
val do_ai : state -> state
