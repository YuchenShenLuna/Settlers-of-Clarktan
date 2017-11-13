(* Building module contains the settlements and cities in the game and
 * informations related to them. *)

(* the type for different settlements or cities *)
type building

(* get_building_list returns the list of buildings currently in game *)
val get_building_list : unit -> building
