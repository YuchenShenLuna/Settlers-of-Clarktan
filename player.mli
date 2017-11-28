open Elements

(* [player] represents a player and the relevant stats. *)
type player = {
  color: color;
  knight: int;
  road_building: int;
  year_of_plenty: int;
  monopoly: int;
  victory_point: int;
  wool: int;
  lumber: int;
  grain: int;
  brick: int;
  ore: int;
  score: int;
  knights_activated: int;
  longest_road: bool;
  largest_army: bool
}


(* [init_player clr] initializes a player identified by color [clr]. *)
val init_player : color -> player
