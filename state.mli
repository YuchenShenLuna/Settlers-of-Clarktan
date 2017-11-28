open Elements

(*****************************************************************************
 *                                DEFINITIONS                                *
 *****************************************************************************)

(* [port] represents a port, which offers a favorable exchange rate. *)
type port = {
  neighbors : int * int;
  demand : resource;
  rate : int
}

(* [canvas] represents the game board. *)
type canvas = {
  tiles : Tile.tile list;
  ports : port list
}

(* [state] represents the state of the game. *)
type state = {
  robber : int;
  deck : DevCard.devcard list;
  turn : color;
  players : Player.player list;
  canvas : canvas
}

(*****************************************************************************
 *                               INITIAL PHASE                               *
 *****************************************************************************)

(* [init_canvas] generates a new board. *)
val init_canvas : unit -> canvas

(* [init_state ()] initializes the game state by setting up the canvas, which
 * includes tiles, ports, and other relevant fields. *)
val init_state : unit -> state

(* [check_initialize_build_settlement ind st] checks whether a settlement
 * can be built at state [st]. *)
val check_initialize_build_settlement : intersection -> state -> bool

(* [check_initialize_build_road road st col] checks whether a road can
 * be built at state [st] for player with color [col]. *)
val check_initialize_build_road : edge -> state -> color -> bool

(* [init_build_settlement ind color st] changes the state when player
 * with color [color] at state [st] builds a settlement at index [ind].
 * raises: Failure "Cannot build settlement at this place" if the player's
 * chosen spot cannot have a settlement built. *)
val init_build_settlement : intersection -> color -> state -> state

(* [init_build_road road color st] changes the state when player
 * with color [color] at state [st] builds a road at index [road].
 * raises : Failure "Cannot build road at this place" if the player's
 * chosen spot cannot have a road built. *)
val init_build_road : edge -> color -> state -> state

(* [init_generate_resources color st] changes the state when in the initial
 * phase the player with color [color] gets the auto-generated resources
 * after building initial settlements and roads at state [st]. *)
val init_generate_resources : color -> state -> state

(*****************************************************************************
 *                                   BUILD                                   *
 *****************************************************************************)

(* returns: if a settlement can be built at given index [ind] for the player
 * identified by color [color] at state [st].
 * check: 1. if the player's number of settlements < 5
          2. there are enough resources [1 lumber, 1 brick, 1 ore, 1 wool]
          3. if check_build_settlements returns true
 * raises: Failure with specific message if the settlement cannot be built
 * at the given index. *)
val can_build_settlement : color -> intersection -> state -> bool

(* returns: if a road can be built at given index [ind] for the player
 * identified by color [color] at state [st].
 * check: 1. if the player's number of roads < 15
          2. there are enough resources [1 lumber, 1 brick]
          3. if check_build_road returns true
 * raises: Failure with specific message if road cannot be built at
 * the given index. *)
val can_build_road : color -> edge -> state -> bool

(* returns: whether a city can be built at given index [ind] for the player
 * identified by color [color] at state [st]
 * check: 1. if the player's number of cities < 4
          2. there are enough resources [3 grains, 2 ores]
          3. if check_build_cities returns true
 * raises: Failure with specific message when city cannot be built at
 * given index *)
val can_build_city : color -> intersection -> state -> bool

(* [build_settlement ind st col] returns the new state after player with color
 * [col] at state [st] builds a new settlement at index [ind].
 * raises: Failure with message relevant to the error of player when settlement
 * cannot be built at the chosen index *)
val build_settlement : intersection -> state -> state

(* [build_road road st col] returns the new state after player with color
 * [col] at state [st] builds a new road at index [road].
 * raises: Failure with message relevant to the error of player when road
 * cannot be built at the chosen index *)
val build_road : edge -> state -> state

(* [build_city ind st col] returns the new state after player with color
 * [col] at state [st] builds a new city at index [ind].
 * raises: Failure with message relevant to the error of player when city
 * cannot be built at the chosen index *)
val build_city : intersection -> state -> state

(* [buy_devcard color st] updates the state [st] when player with color
 * [col] buys a development card using resources.
 * raises: Failure with specific messages when resource card cannot be
 * bought at the time *)
val buy_devcard : state -> state

(*****************************************************************************
 *                                   TRADE                                   *
 *****************************************************************************)

(* [trade_with_player st l1 l2 color st] updates the state when player at
 * state [st] trades with the bank.
 * raises: Failure when player cannot have a valid trade *)
val trade_with_bank :
  color -> (resource * int) list -> (resource * int) list -> state -> state

(* [trade_with_port st l1 l2 color st] updates the state when player at
 * state [st] trades with a port.
 * raises: Failure when player cannot have a valid trade *)
val trade_with_port :
  color -> (resource * int) list -> (resource * int) list -> state -> state

(* [trade_with_player st l1 l2 color st] updates the state when player at
 * state [st] trades with another player.
 * raises: Failure when player cannot have a valid trade *)
val trade_with_player :
  color -> (resource * int) list -> (resource * int) list -> state -> state

(*****************************************************************************
 *                          PLAY A DEVELOPMENT CARD                          *
 *****************************************************************************)

(* [play_robber st i] is the state after the player moves the robber to
 * robber to terrain hex [i]. *)
val play_robber : int -> state -> state

(* [play_knight st color ind] is the state [st] after the current player
 * plays a knight card and moves the robber to terrain hex [ind]. *)
val play_knight : int -> state -> state

(* [play_road_build st color road1 road2] is the state after the current
 * player builds a road at [road1] and another at [road2].
 * raises: Failure when a road cannot be built at the chosen spot. *)
val play_road_build : edge -> edge -> state -> state

(* [play_monopoly st r] is the state after the current player activates the
 * monopoly card at state [st] to acquire resource type [r]. *)
val play_monopoly : resource -> state -> state

(* [play_year_of_plenty st r1 r2] is the state after a player plays a
 * year_of_plenty card at state [st] to acquire resources [r1] [r2]. *)
val play_year_of_plenty : resource -> resource -> state -> state

(*****************************************************************************
 *                                 RESOURCES                                 *
 *****************************************************************************)

(* [tiles_of_roll i] is the list of tiles associated with dice roll [i]. *)
val tiles_of_roll : int -> state -> Tile.tile list

(* [generate_resource st num] generates the resource at dice roll
 * [num] under state [st] and updates the state after generation. *)
val generate_resource : int -> state -> state

(* [discard_resource] discards resources for a player when the robber has been
 * activated. If the player does not have over seven resources, the original
 * state is returned.
 * raises: Failure "you need to discard more resources";
 *         Failure "you need to discard fewer resources" *)
val discard_resource : color -> (resource * int) list -> state -> state

(*****************************************************************************
 *                                  TROPHY                                   *
 *****************************************************************************)

(* [longest_road st] is the player who has the longest road achievement. *)
val longest_road : state -> state

(* [largest_army st] is the player who has the largest army achivement. *)
val largest_army : state -> state

(*****************************************************************************
 *                                    DO                                     *
 *****************************************************************************)

(* [end_turn st] is the game state at the end of a turn.
 * raises: Not_found if the player whose turn it is does not exist. *)
val end_turn : state -> state

(* [do_player cmd color st] is the game state after a command [cmd] is
 * executed. *)
val do_player : Command.command -> color option -> state -> state

(*****************************************************************************
 *                                   TEST                                    *
 *****************************************************************************)

(* [score color st] is the score of the player identified by color [color]. *)
val score : color -> state -> int

(* [check_win color st] indicates whether the player identified by color [color]
 * has won the game *)
val check_win : color -> state -> bool

(* [settlements color st] is a list of indices that represent settlements
 * built by the player identified by color [color]. *)
val settlements : color -> state -> intersection list

(* [cities color st] is a list of indices that represent cities built by the
 * player identified by color [color]. *)
val cities : color -> state -> intersection list

(* [roads color st] is a list of pairs of indices that represent roads built
 * by the player identified by color [color]. *)
val roads : color -> state -> edge list

(* [ports color st] is a list of pairs of indices that represent ports
 * accessible by the player identified by color [color]. *)
val ports : color -> state -> edge list

(* [cards color st] is a list of the development cards held by the player
 * identified by color [color]. *)
val cards : color -> state -> (DevCard.devcard * int) list

(* [resources color st] is a list of the resources held by the player
 * identified by color [color]. *)
val resources : color -> state -> (resource * int) list

(* [robber st] is current location of the robber. *)
val robber : state -> int

(* [turn st] is the color of the player whose turn it is. *)
val turn : state -> color
