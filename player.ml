open Elements

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

let init_player color =
  {
    color = color;
    dev_list = [];
    res_list = [];
    score = 0;
    knights = 0;
    longest_road = false;
    largest_army = false
  }
