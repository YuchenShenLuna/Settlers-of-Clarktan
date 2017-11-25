open Elements

(* [init_ai color] initializes the ai player and his information *)
val init_ai : color -> Player.player

(* [init_ai_list num] initializes the list of ai players for the game.
 * The number of ai players depend on [num]. *)
val init_ai_list : int -> Player.player list
