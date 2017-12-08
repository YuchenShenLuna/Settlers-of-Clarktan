open Elements

(*****************************************************************************
 *                                DEFINITIONS                                *
 *****************************************************************************)

(* [port] represents a port, which offers a favorable exchange rate. *)
type port = {
  neighbors : int * int;
  demand : resource option;
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
  deck : card list;
  turn : color;
  players : Player.player list;
  canvas : canvas
}

(*****************************************************************************
 *                               INITIAL PHASE                               *
 *****************************************************************************)

(* [init_state ()] initializes the game state; it pseudo-randomly selects
 * fields, such as what resources and numbered tokens to place upon the tiles
 * and the order of play. *)
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
(* [fetch_neighbors i] fetches the neighboring intersections of the
 * settlement with index [i]*)
val fetch_neighbors : int -> int list

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

(* [check_build_settlement num st color] checks whether a settlement can be
 * build at index [num] at state [st] for player with color [color]. This
 * checks whether a settlement follows the rule of no two settlements have
 * fewer than two roads in between and settlement must have own color's
 * road in one end, and whether the settlement is build upon an empty place *)
val check_build_settlement : int -> state -> color -> bool

(* [check_build_road road st col] checks whether a road can be build at index
 * [road] at state [st] for player identified by color [col]. This only checks
 * the distance rules, not resources nor availability. *)
val check_build_road : edge -> state -> color -> bool

(* [check_build_cities num st color] checks whether a city can be
 * build at index [num] at state [st] for player with color [color]. This
 * checks whether a city follows the rule of being build upon a settlement
 * that is of the same color *)
val check_build_cities : int -> state -> color -> bool

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

(* [buy_card color st] updates the state [st] when player with color
 * [col] buys a development card using resources.
 * raises: Failure with specific messages when resource card cannot be
 * bought at the time *)
val buy_card : state -> state

(*****************************************************************************
 *                                   TRADE                                   *
 *****************************************************************************)

(* [best_rate resource color state] is the best maritime trading rate for
 * the resource [resource] and the player with color [color]. *)
val best_rate : resource -> color -> state -> int

val add_resources : (resource * int) list -> color -> state -> state

val remove_resources : (resource * int) list -> color -> state -> state

val trade_ok : (resource * int) list -> (resource * int) list -> color option
  -> state -> bool

val domestic : (resource * int) list -> (resource * int) list -> color
  -> state -> state

val maritime : (resource * int) list -> (resource * int) list -> state -> state

(* [list_of_resources color st] returns the list of resources under state [st]
 * for player identified by color [color] *)
val list_of_resources : color -> state -> state

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

(* [num_resource c r s] is the number of resources of type [r] held by the
 * player identified by color [c]. *)
val num_resource : color -> resource -> state -> int

(* [num_all_resources c r s] is the total number of resources held by the
 * player identified by color [c]. *)
val num_all_resources : color -> state -> int

(* [generate_resource st num] generates the resource at dice roll
 * [num] under state [st] and updates the state after generation. *)
val generate_resource : int -> state -> state

(*****************************************************************************
 *                              ACHIEVEMENTS                                 *
 *****************************************************************************)

val get_player : color -> state -> Player.player

val longest_road_length :  state -> color ->int

(* [longest_road st] is the player who has the longest road achievement. *)
val longest_road : state -> state

(* [largest_army st] is the player who has the largest army achivement. *)
val largest_army : state -> state

(*****************************************************************************
 *                                    DO                                     *
 *****************************************************************************)

(* [end_turn flag st] is the game state at the end of a turn. The boolean [flag]
 * indicates who the next player is.
 * raises: Not_found if the player whose turn it is does not exist. *)
val end_turn : bool -> state -> state

(* [do_move cmd color st] is the game state after a command [cmd] is
 * executed. *)
val do_move : Command.command -> color option -> state -> state

(*****************************************************************************
 *                                   TEST                                    *
 *****************************************************************************)

(* [state_to_test] is a game state. *)
val state_to_test : state

(* [score color st] is the score of the player identified by color [color]. *)
val score : color -> state -> int

(* [check_win color st] indicates whether the player identified by color [color]
 * has won the game. *)
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
val cards : color -> state -> (card * int) list

(* [resources color st] is a list of the resources held by the player
 * identified by color [color]. *)
val resources : color -> state -> (resource * int) list

(* [robber st] is current location of the robber. *)
val robber : state -> int

(* [turn st] is the color of the player whose turn it is. *)
val turn : state -> color
