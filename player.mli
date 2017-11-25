open Elements

(* Player module handles the player informations for the game *)

(* player is the type of players and informations they hold *)
type player =
  {
    color: color;
    dev_list: DevCard.devcard list;
    res_list: resource list;
    score: int;
    knights: int;
    longest_road: bool;
    largest_army: bool
  }

(* [init_human_player color] initializes the human player and his information *)
val init_player : color -> player
