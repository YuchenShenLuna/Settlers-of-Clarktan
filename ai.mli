open Tile

open DevCard

open Building

(* [init_ai color] initializes the ai player and his information *)
val init_ai : color -> player

(* [init_ai_list num] initializes the list of ai players for the game.
 * The number of ai players depend on [num]. *)
val init_ai_list : int -> player list
