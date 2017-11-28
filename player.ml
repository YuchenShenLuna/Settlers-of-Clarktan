open Elements

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

let init_player clr = {
  color = clr;
  knight = 0;
  road_building = 0;
  year_of_plenty = 0;
  monopoly = 0;
  victory_point = 0;
  wool = 0;
  lumber = 0;
  grain = 0;
  brick = 0;
  ore = 0;
  score = 0;
  knights_activated = 0;
  longest_road = false;
  largest_army = false
}
