open Tile

open DevCard

open Building

(* Player module handles the player informations for the game *)

(* color is the type of different colors for different players *)
type color

(* player is the type of players and informations they hold *)
type player

(* [init_human_player color] initializes the human player and his information *)
val init_human_player : color -> player

(* [init_ai color] initializes the ai player and his information *)
val init_ai : color -> player

(* [init_ai_list num] initializes the list of ai players for the game.
 * The number of ai players depend on [num]. *)
val init_ai_list : int -> player list

(* [update_resource p] updates player [p]'s resources *)
val update_resource : player -> resource -> player

(* [update_card p] updates player [p]'s development cards *)
val update_card : player -> devcard -> player

(* [update_buildings p] updates the buildings list of player [p] *)
val update_buildings : player -> building -> player

(* [update_trophy p] updates trophies for player [p] *)
val update_trophy : player -> player

(* [update_score p] updates the score field for player [p] *)
val update_score : player -> player

(* [update_roads p] update the roads for player [p] *)
val update_roads : player -> player
