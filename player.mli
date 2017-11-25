open Elements

(* Player module handles the player informations for the game *)

(* player is the type of players and informations they hold *)
type player =
  {
    color: color;
    knight: int;
    road_of_Building: int;
    year_of_Plenty: int;
    monopoly: int;
    victory_Point: int;
    wool: int;
    lumber: int;
    grain: int;
    brick: int;
    ore: int;
    score: int;
    knights: int;
    longest_road: bool;
    largest_army: bool
  }


(* [init_human_player color] initializes the human player and his information *)
val init_player : color -> player
