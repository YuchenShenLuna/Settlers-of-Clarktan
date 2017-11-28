open Elements

(* Command module takes in player's command, either through mouse click
 * or terminal entry, and parses them accordingly. *)

(* command represents possible types of commands *)
type command =
  | Setup of int * road
  | BuildSettlement of int
  | BuildCity of int
  | BuildRoad of road
  | BuyCard
  | Knight of int
  | RoadBuilding of road * road
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

(* [parse str] parses the command given by [str] to type command *)
val parse_text : Tile.tile list -> string -> command
