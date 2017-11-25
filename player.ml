open Elements

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

let init_player color =
  {
    color = color;
    knight=0;
    road_of_Building=0;
    year_of_Plenty=0;
    monopoly=0;
    victory_Point=0;
    wool=0;
    lumber=0;
    grain=0;
    brick=0;
    ore=0;
    score = 0;
    knights = 0;
    longest_road = false;
    largest_army = false
  }
