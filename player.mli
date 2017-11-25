(* Player module handles the player informations for the game *)

(* color is the type of different colors for different players *)
type color

(* player is the type of players and informations they hold *)
type player

(* [init_human_player color] initializes the human player and his information *)
val init_player : color -> player
