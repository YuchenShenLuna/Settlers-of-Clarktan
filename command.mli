open Elements

(* [command] represents a command input by a player. *)
type command =
  | Start
  | InitSettlement of intersection
  | InitRoad of edge
  | BuildSettlement of intersection
  | BuildCity of intersection
  | BuildRoad of edge
  | BuyCard
  | PlayKnight of int
  | PlayRoadBuilding of edge * edge
  | PlayYearOfPlenty of resource * resource
  | PlayMonopoly of resource
  | Robber of int
  | DomesticTrade of bool * (resource * int) list * (resource * int) list
  | MaritimeTrade of bool * (resource * int) * (resource * int)
  | Discard of (resource * int) list
  | EndTurn
  | Quit
  | Invalid

(* [nearby_intersection tiles (x, y)] is Some intersection (i.e., hex corner)
 * near the coordinates (x, y), if there is one, and None, otherwise. *)
val nearby_intersection : Tile.tile list -> float * float -> int option

(* [nearby_edge tiles (x, y)] is Some edge (i.e., hex edge) near the
 * coordinates (x, y), if there is one, and None, otherwise. *)
val nearby_edge : Tile.tile list -> float * float -> edge option

(* [parse_mouse_click ()] waits for a mouse event and returns the coordinates
 * of the first mouse click to occur. *)
val parse_mouse_click : unit -> float * float

(* [parse str] is the command that represents the input [str]. *)
val parse_text : Tile.tile list -> string -> command

val string_of_command : command -> string
