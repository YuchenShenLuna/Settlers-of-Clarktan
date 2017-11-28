open Elements

(* [command] represents a command input by a player. *)
type command =
  | Setup of int * edge
  | BuildSettlement of int
  | BuildCity of int
  | BuildRoad of edge
  | BuyCard
  | Knight of int
  | RoadBuilding of edge * edge
  | YearOfPlenty of resource * resource
  | Monopoly of resource
  | Robber of int
  | DomesticTrade of (resource * int) list * (resource * int) list
  | MaritimeTrade of (resource * int) * (resource * int)
  | Accept of bool
  | Discard of (resource * int) list
  | EndTurn
  | Quit
  | Invalid

(* [parse_mouse_click ()] waits for a mouse event and returns the coordinates
 * of the first mouse click to occur. *)
val parse_mouse_click : unit -> float * float

(* [parse str] is the command that represents the input [str]. *)
val parse_text : Tile.tile list -> string -> command
