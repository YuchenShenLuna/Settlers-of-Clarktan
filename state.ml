open Elements
open Player
open Command

type canvas = {
  tiles: Tile.tile list;
  ports: port list
}

type state = {
  robber: int;
  deck: DevCard.devcard list;
  turn : color;
  players: Player.player list;
  canvas : canvas
}

(* [roll ()] generates a random dice roll in range of [2..12] for random
 * tile dice number at the initialization of the game *)
let rec roll () =
  let i = 2 + Random.int 11 in
  if i <> 7 then i else roll ()

(* [random_resource ()] generates a random resource by using random number *)
let random_resource () =
  let open Tile in
  match Random.int 5 with
  | 0 -> Lumber
  | 1 -> Wool
  | 2 -> Grain
  | 3 -> Brick
  | _ -> Ore

let init_canvas () =
  let center = 500., 375. in
  let length = 50. in
  let apothem =  length *. sqrt 3. /. 2. in
  let open Tile in
  {
    tiles = [
      { indices = [3; 4; 15; 14; 13; 2];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center +. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [5; 6; 17; 16; 15; 4];
        dice = roll ();
        resource = random_resource ();
        center = fst center, snd center +. 3. *. length ;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [7; 8; 19; 18; 17; 6];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center +. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [13; 14; 25; 24; 23; 12];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [15; 16; 27; 26; 25; 14];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [17; 18; 29; 28; 27; 16];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [19; 20; 31; 30; 29; 18];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [23; 24; 35; 34; 33; 22];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 4. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [25; 26; 37; 36; 35; 24];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [27; 28; 39; 38; 37; 26];
        dice = roll ();
        resource = random_resource ();
        center = fst center, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [29; 30; 41; 40; 39; 28];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [31; 32; 43; 42; 41; 30];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 4. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [35; 36; 47; 46; 45; 34];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [37; 38; 49; 48; 47; 36];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [39; 40; 51; 50; 49; 38];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [41; 42; 53; 52; 51; 40];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [47; 48; 51; 58; 57; 46];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center -. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [49; 50; 61; 60; 59; 48];
        dice = roll ();
        resource = random_resource ();
        center = fst center, snd center -. 3. *. length ;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [51; 52; 63; 62; 61; 50];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center -. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
    ];
    ports = [
      {
        neighbors = (2,3);
        resource =random_resource ();
        rate=2;
      };
      {
        neighbors = (5,6);
        resource =random_resource ();
        rate=3;
      };
      {
        neighbors = (12,23);
        resource =random_resource ();
        rate=2;
      };
      {
        neighbors = (19,20);
        resource =random_resource ();
        rate=3;
      };
      {
        neighbors = (32,43);
        resource =random_resource ();
        rate=2;
      };
      {
        neighbors = (34,45);
        resource =random_resource ();
        rate=2;
      };
      {
        neighbors = (52,53);
        resource =random_resource ();
        rate=2;
      };
      {
        neighbors = (57,58);
        resource =random_resource ();
        rate=3;
      };
      {
        neighbors = (60,61);
        resource =random_resource ();
        rate=3;
      };
    ]
  }

let init_state () =
  let open DevCard in
{
  robber = 19;
  deck =
    [
      Knight; VictoryPoint; Knight; RoadBuilding;YearOfPlenty; Knight;
      RoadBuilding; Knight; Knight; VictoryPoint; Knight; Knight; Monopoly;
      Knight; YearOfPlenty; Knight; Knight; VictoryPoint; Knight;
      Knight; Knight; Monopoly; VictoryPoint; Knight; VictoryPoint
    ];
  turn = White;
  players =
    [
      Player.init_player Red; Player.init_player Yellow;
      Player.init_player Blue; Player.init_player Green
    ];
  canvas = init_canvas ()
}

let end_turn st =
  let rec index acc = function
    | [] -> raise Not_found
    | h :: t -> if h.color = st.turn then acc else index (1 + acc) t
  in
  let turn = (List.nth st.players ((index 0 st.players + 1) mod 4)).color in
  {st with turn}

(* [fetch_neighbors num] fetches neigboring indexes for the settlement
 * with index [num]*)
let fetch_neighbors num =
  let fetch num =
    if num mod 2 = 1 then [num-11; num-1; num+1]
    else [num+11; num-1; num+1]
  in
  let lst = fetch num in
  let possible_index =
    [2; 3; 4; 5; 6; 7; 8; 12; 13; 14; 15; 16; 17; 18; 19; 20; 22; 23; 24; 25;
     26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43;
     45; 46; 47; 48; 49; 50; 51; 52; 53; 57; 58; 59; 60; 61; 62; 63]
  in
  List.filter (fun x -> List.mem x possible_index) lst

let init_build_settlement ind color st =
  let open Tile in
  let check_initialize_build_settlement ind st =
    let neighbors = fetch_neighbors ind in
    let tiles = st.canvas.tiles in
    let rec help lst num =
      match lst with
      | [] -> White
      | h::t ->
        match List.assoc_opt num h.buildings with
        | None -> help t num
        | Some (color, _) -> color
    in
    if help tiles ind <> White then
      false
    else
      let res = List.fold_left
          (fun acc x -> acc && help tiles x = White) true neighbors in
      if res = false then false else true
  in
  let b = check_initialize_build_settlement ind st in
  if b = false then
    failwith "Cannot build settlement at this place"
  else
    let new_tiles =
      st.canvas.tiles
      |> List.map (fun x -> if List.mem ind x.indices = false then x
                    else {x with buildings = (ind, (color, 1))::x.buildings})
    in
    {st with canvas = {tiles = new_tiles; ports = st.canvas.ports}}

let init_build_road (i0, i1) color st =
  let open Tile in
  let check_initialize_build_road (a, b) st color =
    let tiles = st.canvas.tiles in
    let rec help lst num =
      match lst with
      | [] -> White
      | h::t ->
        match List.assoc_opt num h.buildings with
        | None -> help t num
        | Some (color, _) -> color
    in
    if help tiles i0 <> color && help tiles i1 <> color then
      false
    else
      let rec help' lst x y =
        match lst with
        | [] -> White
        | h::t ->
          if List.mem_assoc (x, y) h.roads then
            List.assoc (x, y) h.roads
          else if List.mem_assoc (y, x) h.roads then
            List.assoc (y, x) h.roads
          else
            help' t x y
      in
      let res = help' tiles a b = White in
      if res = false then false else true
  in
  let b = check_initialize_build_road (i0, i1) st color in
  if b = false then
    failwith "Cannot build road at this place"
  else
    let new_tiles =
      st.canvas.tiles
      |> List.map (fun x -> if List.mem i0 x.indices = false
                              || List.mem i1 x.indices = false then x
                            else {x with roads = ((i0, i1), color)::x.roads})
    in {st with canvas = {tiles = new_tiles; ports = st.canvas.ports}}

let init_generate_resources color st =
  let open Tile in
  let info =
    st.canvas.tiles
    |> List.map (fun x -> (x.buildings, x.resource))
    |> List.map (fun (lst, r) -> List.map (fun (_, (c, _)) -> r, c) lst)
    |> List.flatten
    |> List.filter (fun (r, c) -> c=color)
  in
  let count res =
    List.fold_left (fun acc (r, c) -> if res=r then acc+1 else acc) 0 info in
  let new_players =
    st.players
    |> List.map
      (fun x -> if x.color <> color then x
        else {x with
              wool = x.wool + count Wool;
              brick = x.brick + count Brick;
              lumber = x.lumber + count Lumber;
              ore = x.ore + count Ore;
              grain = x.grain + count Grain})
  in {st with players = new_players}

(* [check_build_settlements num st color] checks whether a settlement can be
 * build at index [num] at state [st] for player with color [color]. This
 * checks whether a settlement follows the rule of no two settlements have
 * fewer than two roads in between and settlement must have own color's
 * road in one end, and whether the settlement is build upon an empty place *)
let check_build_settlements num st color =
  let open Tile in
  let ind_lst = fetch_neighbors num in
  let tile_lst = st.canvas.tiles in
  let rec help lst num =
    match lst with
    | [] -> White
    | h::t ->
      match List.assoc_opt num h.buildings with
      | None -> help t num
      | Some (color, _) -> color
  in
  if help tile_lst num <> White then false
  else
    let res =
      List.fold_left (fun acc x -> acc && help tile_lst x = White) true ind_lst in
    if res = false then false
    else
      let rec help' lst num2 =
        match lst with
        | [] -> White
        | h::t ->
          if List.mem_assoc (num, num2) h.roads then
            List.assoc (num, num2) h.roads
          else if List.mem_assoc (num2, num) h.roads then
            List.assoc (num2, num) h.roads
          else
            help' t num2
      in
      List.fold_left (fun acc x -> acc || help' tile_lst x = color) false ind_lst

let check_build_road (i0, i1) st color =
  let open Tile in
  let ind_lst_i0 = List.filter (fun x -> x <> i0 && x <> i1)
      (fetch_neighbors i0) in
  let ind_lst_i1 = List.filter (fun x -> x <> i0 && x <> i1)
      (fetch_neighbors i1) in
  let tile_lst = st.canvas.tiles in
  let rec help' lst x y =
    match lst with
    | [] -> White
    | h::t ->
      if List.mem_assoc (x, y) h.roads
      then List.assoc (x, y) h.roads
      else if List.mem_assoc (y, x) h.roads
      then List.assoc (y, x) h.roads
      else help' t x y
  in
  let res = help' tile_lst i0 i1 = White in
  if res = false then false
  else
  let rec help lst num =
    match lst with
    | [] -> White
    | h::t ->
      match List.assoc_opt num h.buildings with
      | None -> help t num
      | Some (color, _) -> color
  in
  let res' = help tile_lst i0 = color || help tile_lst i1 = color in
  if res' = true then
    true
  else
    List.fold_left (fun acc x ->
        acc || help' tile_lst i0 x = color) false ind_lst_i0
    || List.fold_left (fun acc x ->
        acc || help' tile_lst i1 x = color) false ind_lst_i1

(* [check_build_cities num st color] checks whether a city can be
 * build at index [num] at state [st] for player with color [color]. This
 * checks whether a city follows the rule of being build upon a settlement
 * that is of the same color *)
let check_build_cities num st color =
  let open Tile in
  let tile_lst = st.canvas.tiles in
  let rec help lst num =
    match lst with
    | [] -> White, 1
    | h::t ->
      match List.assoc_opt num h.buildings with
      | None -> help t num
      | Some (color, ty) -> (color, ty)
  in
  help tile_lst num = (color, 1)

let fetch_tiles num tiles =
  List.filter (fun x -> x.Tile.dice = num) tiles

let play_road_build st color (i0, i1) (j0, j1) =
  let open Tile in
  let player = List.filter (fun x -> x.color = color) st.players |> List.hd in
  if player.road_building < 1 then
    failwith "No Road Building Card"
  else
    if check_build_road (i0, i1) st color = false then
      failwith "Cannot Build Road at given first place"
  else
    let new_tiles =
      List.map (fun t -> if List.mem i0 t.indices && List.mem i1 t.indices then
                           {t with roads=((i0, i1), color)::t.roads}
                         else t) st.canvas.tiles in
    let new_players =
      List.map (fun x -> if x <> player then x
                 else {player with road_building = player.road_building-1})
                    st.players in
    let st' = {st with canvas = {tiles = new_tiles; ports=st.canvas.ports};
                       players = new_players}
    in
    if check_build_road (j0, j1) st' color = false then
      failwith "Cannot Build Road at given second place"
    else
    let new_tiles' =
      List.map (fun t -> if List.mem i0 t.indices && List.mem i1 t.indices then
                           {t with roads=((i0, i1), color)::t.roads}
                 else t) st'.canvas.tiles in
    {st' with canvas = {tiles = new_tiles'; ports=st.canvas.ports}}

(* [calc_score st color] calculates the score for player with color [color]
 * at state [st] *)
let calc_score st color =
  let open Tile in
  let build_score =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.filter (fun (ind, (c, rate)) -> c=color)
    |> List.map (fun (ind, (c, rate)) -> rate)
    |> List.fold_left (fun acc x -> acc + x) 0
  in
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  let victory_card_score = player.victory_point * 2 in
  let longest_road_score = if player.longest_road then 2 else 0 in
  let largest_army_score = if player.largest_army then 2 else 0 in
  build_score + victory_card_score + longest_road_score + largest_army_score

let check_win st color =
  calc_score st color >= 10

(* need [1 grain, 1 ore, 1 wool] *)
let buy_devcard color st =
  let open DevCard in
  let player = List.hd (List.filter (fun x -> x.color=color) st.players) in
  if player.grain < 1 || player.ore < 1 || player.wool < 1 then
    failwith "Not enough resource to buy development card"
  else if List.length st.deck < 1 then
    failwith "Development cards sold out"
  else
    let card_lst = (shuffle st.deck) in
    let card = List.hd card_lst in
    let rest = List.tl card_lst in
    let updated_players col lst =
      List.map (fun x -> if x.color <> col then x
                 else
                   match card with
                   | Knight -> {x with knight = x.knight+1;
                                       grain = x.grain-1;
                                       ore = x.ore-1;
                                       wool = x.wool-1}
                   | RoadBuilding -> {x with road_building = x.road_building+1;
                                                 grain = x.grain-1;
                                                 ore = x.ore-1;
                                                 wool = x.wool-1}
                   | YearOfPlenty -> {x with year_of_plenty = x.year_of_plenty+1;
                                               grain = x.grain-1;
                                               ore = x.ore-1;
                                               wool = x.wool-1}
                   | Monopoly -> {x with monopoly = x.monopoly+1;
                                         grain = x.grain-1;
                                         ore = x.ore-1;
                                         wool = x.wool-1}
                   | VictoryPoint -> {x with victory_point = x.victory_point+1;
                                              grain = x.grain-1;
                                              ore = x.ore-1;
                                              wool = x.wool-1}) lst
    in
    {st with deck = rest; players = updated_players color st.players}

(* [num_resources player] returns the number of resources for player [player] *)
let num_resources player = function
  | Lumber -> player.lumber
  | Wool   -> player.wool
  | Grain  -> player.grain
  | Brick  -> player.brick
  | Ore    -> player.ore
  | Null   -> 0

(* [check_num_resources col st] returns the number of resources the player
 * with color [color] has at state [st] *)
let check_num_resources color st =
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  num_resources player Lumber +
  num_resources player Wool +
  num_resources player Grain +
  num_resources player Brick +
  num_resources player Ore

let discard_resource color st lst =
  if check_num_resources color st < 7 then st
  else
    let num_in_lst = List.fold_left (fun acc (a, b) -> acc + b) 0 lst in
    if num_in_lst < check_num_resources color st then
      failwith "you need to discard more resources"
    else if num_in_lst > check_num_resources color st then
      failwith "you need to discard fewer resources"
    else
      let new_players =
        st.players
        |> List.map
          (fun x -> if x.color <> color then x
            else {x with wool = if List.assoc_opt Wool lst <> None then
                                     x.wool - (List.assoc Wool lst)
                                   else x.wool;
                         lumber = if List.assoc_opt Lumber lst <> None then
                                    x.wool - (List.assoc Lumber lst)
                                  else x.wool;
                         grain = if List.assoc_opt Grain lst <> None then
                                    x.wool - (List.assoc Grain lst)
                                 else x.wool;
                         brick = if List.assoc_opt Brick lst <> None then
                                   x.wool - (List.assoc Brick lst)
                                  else x.wool;
                         ore = if List.assoc_opt Ore lst <> None then
                                x.wool - (List.assoc Ore lst)
                               else x.wool;})
      in {st with players = new_players}

let play_robber st color ind =
  let open Tile in
  let pos_stealees =
    (List.nth st.canvas.tiles ind).buildings
    |> List.map (fun (_, (col, _)) -> col)
    |> List.sort_uniq compare
    |> List.filter (fun x -> check_num_resources x st > 0) in
  let shuffle lst =
    let i = Random.int (List.length lst) in
    List.nth lst i
  in
  let stealee_color = shuffle pos_stealees in
  let stealee =
    List.hd (List.filter (fun x -> x.color = stealee_color) st.players) in
  let pos_wool = if stealee.wool > 0 then [Wool] else [] in
  let pos_lumber = if stealee.lumber > 0 then [Lumber] else [] in
  let pos_brick = if stealee.wool > 0 then [Brick] else [] in
  let pos_ore = if stealee.lumber > 0 then [Ore] else [] in
  let pos_grain = if stealee.wool > 0 then [Grain] else [] in
  let pos_resource = pos_wool @ pos_brick @ pos_lumber @ pos_grain @ pos_ore in
  let stolen_resource = shuffle pos_resource in
  let new_players =
    st.players
    |> List.map (fun x -> if x.color = color then
                    begin
                      match stolen_resource with
                      | Wool -> {x with wool = x.wool+1}
                      | Brick -> {x with brick = x.brick+1}
                      | Lumber -> {x with lumber = x.lumber+1}
                      | Ore -> {x with ore = x.ore+1}
                      | Grain -> {x with grain = x.grain+1}
                      | Null -> x end
                  else if x.color = stealee_color then
                    begin
                      match stolen_resource with
                      | Wool -> {x with wool = x.wool-1}
                      | Brick -> {x with brick = x.brick-1}
                      | Lumber -> {x with lumber = x.lumber-1}
                      | Ore -> {x with ore = x.ore-1}
                      | Grain -> {x with grain = x.grain-1}
                      | Null -> x end
                      else x)
  in {st with players = new_players;
              robber = ind}

let play_knight st color ind =
  let st' = play_robber st color ind in
  let new_players =
    List.map (fun x -> if x.color <> color then x
               else
                 {x with knight = x.knight-1;
                         knights_activated = x.knights_activated+1}) st'.players
  in {st' with players = new_players}

let can_build_settlements ind st color =
  let open Tile in
  let num_settlements =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.filter (fun (x, (y, z)) -> y=color && z=1)
    |> List.length
  in
  if num_settlements >= 5 then
    failwith "You have build the maximum number of settlements possible"
  else if check_build_settlements ind st color = false then
    failwith "You cannot build settlement at this place"
  else
    let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
    if player.lumber < 1 || player.brick < 1
       || player.ore < 1 || player.wool < 1 then
      failwith "You do not have enough resource to build a settlement"
    else true

let can_build_road (i0, i1) st color =
  let open Tile in
  let num_roads =
    st.canvas.tiles
    |> List.map (fun x -> x.roads)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.filter (fun (x, y) -> y=color)
    |> List.length
  in
  if num_roads >= 15 then
    failwith "You have build the maximum number of roads possible"
  else if check_build_road (i0, i1) st color = false then
    failwith "You cannot build road at this place"
  else
    let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
    if player.lumber < 1 || player.brick < 1 then
      failwith "You do not have enough resource to build a road"
    else true

let can_build_city ind st color =
  let open Tile in
  let num_cities =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.filter (fun (x, (y, z)) -> y=color && z=2)
    |> List.length
  in
  if num_cities >= 5 then
    failwith "You have build the maximum number of cities possible"
  else if check_build_cities ind st color = false then
    failwith "You cannot build city at this place"
  else
    let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
    if player.grain < 3 || player.ore < 2 then
      failwith "You do not have enough resource to build a city"
    else true

let build_settlement ind st color =
  let open Tile in
  let _ = can_build_settlements ind st color in
  let new_players =
    st.players
    |> List.map (fun x -> if x.color <> color then x
                  else {x with lumber = x.lumber-1;
                               brick = x.brick-1;
                               ore = x.ore-1;
                               wool = x.wool-1})
  in
  let new_tiles =
    st.canvas.tiles
    |> List.map (fun x -> if List.mem ind x.indices = false then x
                  else {x with buildings = (ind, (color, 1))::x.buildings})
  in
  {st with players = new_players;
           canvas = {tiles = new_tiles;
                     ports = st.canvas.ports}}

let build_road (i0, i1) st color =
  let open Tile in
  let _ = can_build_road (i0, i1) st color in
  let new_players =
    st.players
    |> List.map (fun x -> if x.color <> color then x
                  else {x with lumber = x.lumber-1;
                               brick = x.brick-1})
  in
  let new_tiles =
    st.canvas.tiles
    |> List.map (fun x -> if List.mem i0 x.indices = false
                          || List.mem i1 x.indices = false then x
                          else {x with roads = ((i0, i1), color)::x.roads})
  in
  {st with players = new_players;
           canvas = {tiles = new_tiles;
                     ports = st.canvas.ports}}

let build_city ind st color =
  let open Tile in
  let _ = can_build_city ind st color in
  let new_players =
    st.players |>
    List.map (fun x -> if x.color <> color then x
               else {x with grain = x.grain-3; ore = x.ore-2})
  in
  let new_tiles =
    st.canvas.tiles
    |> List.map (fun x -> if List.mem ind x.indices = false then x
                  else {x with buildings = List.map (fun (a, (b, c)) ->
                      if a = ind then (a, (b, 2)) else (1, (b, c))) x.buildings})
  in
  {st with players = new_players;
           canvas = {tiles = new_tiles; ports = st.canvas.ports}}

(* [check_robber tile st] checks whether a robber is at tile [tile] under
 * state [st] *)
let check_robber tile st =
  let robber_ind = st.robber in
  if robber_ind > 18 || robber_ind < 0 then
    true
  else
    let tile_at_ind = List.nth st.canvas.tiles robber_ind in
    tile_at_ind = tile

let generate_resource st num =
  let open Tile in
  let tiles = fetch_tiles num st.canvas.tiles in
  let info =
    List.flatten
      (List.map
         (fun x -> if check_robber x st then []
           else List.map
               (fun (a, (b, c)) -> (b, (x.resource, c))) x.buildings) tiles) in
  let rec help st lst =
    match lst with
    | [] -> st
    | (col, (resource, mul))::t ->
      begin
        let rec new_players playerlst acc =
        match playerlst with
        | [] -> acc
        | h::t ->
          if h.color = col then
            let newp =
            begin
              match resource with
              | Lumber -> {h with lumber = h.lumber + mul}
              | Wool -> {h with wool = h.wool + mul}
              | Grain -> {h with grain = h.grain + mul}
              | Brick -> {h with brick = h.brick + mul}
              | Ore -> {h with ore = h.ore + mul}
              | Null -> h
            end
            in new_players t (newp::acc)
          else
            new_players t (h::acc)
        in help {st with players = new_players st.players []} t
      end
  in help st info

(* [fetch_road st] fetches the roads at given state [st]*)
let fetch_roads st =
  let cmp (s1, e1) (s2, e2) =
    if s1 < s2 then -1
    else if s1 > s2 then 1
    else if e1 < e2 then -1
    else if e1 > e2 then 1
    else 0
  in
  let open Tile in
  List.fold_left
    (
    fun acc t ->
      acc @ List.mapi (
        fun i j ->
          let k = List.nth t.indices ((i + 1) mod 6) in
          if j < k then j, k else k, j
    ) t.indices
  ) [] st.canvas.tiles |> List.sort_uniq cmp

(* [find_possible_owner_of_road_one_tile st rd_list road] finds all possible
 * owners of the given road [road] at state [st] *)
let rec find_possible_owner_of_road_one_tile st rd_list (s,e)=
  let open Tile in
  match rd_list with
  | [] -> None
  | h::t ->
    if (s,e) = fst h then
      Some (List.find (fun p -> p.color = snd h) st.players)
    else
      find_possible_owner_of_road_one_tile st t (s,e)

(* [find_owner_of_road st rd] finds the owner of the road [rd] at state [st] *)
let find_owner_of_road st rd =
  let open Tile in
  List.fold_left (fun acc x ->
      if find_possible_owner_of_road_one_tile st (x.roads) rd <> None then
        find_possible_owner_of_road_one_tile st x.roads rd
      else acc) (None) (st.canvas.tiles)

(* [get_player_out_of_some pl] extracts player from the player option [pl] *)
let get_player_out_of_some pl=
  match pl with
  | Some p -> p
  | None -> failwith "impossible"

let longest_road st=
  let edges = fetch_roads st in
  let successors n e =
    List.map (fun (_, v) -> v) (List.filter (fun (u, _) -> n = u) e)
  in
  let dfs graph start  =
    let rec rdfs visited node =
      if not (List.mem node visited) then
        begin
          let s = successors node graph in
          List.fold_left rdfs (node::visited) s
        end

      else visited
    in rdfs [] start in
  let longest_road=dfs edges (fst (List.hd edges)) in
  if (List.length longest_road)-1 <5 then st else
    let possible_player=find_owner_of_road st ((List.hd longest_road),(List.nth longest_road 1 ))
    in if possible_player <> None then
      let now_player=get_player_out_of_some possible_player in
      let updated_player={now_player with longest_road=true} in
      let updated_player_list=List.map (fun x-> if x.color!=updated_player.color
                                         then {x with longest_road=false} else
                                           updated_player) st.players in
      {st with players=updated_player_list}
    else
      let now_player'=get_player_out_of_some (find_owner_of_road st ((List.nth longest_road 1 ),(List.hd longest_road))) in
      let updated_player'={now_player' with longest_road=true} in
      let updated_player_list'=List.map (fun x->
          if x.color!=updated_player'.color
          then {x with longest_road=false}
          else updated_player') st.players in
      {st with players=updated_player_list'}

let largest_army st =
  let possible_player=
    List.fold_left
      (fun acc x ->
         match acc with
         | None -> if x.knights_activated >= 3 then Some x else None
         | Some y ->
           if x.knights_activated > y.knights_activated
           then Some x else acc
      ) None st.players
  in if possible_player = None then st else
    let now_player= get_player_out_of_some possible_player  in
    let updated_player={now_player with largest_army=true} in
    let updated_player_list=List.map (fun x->
        if x.color!=updated_player.color
        then {x with largest_army=false}
        else updated_player) st.players in
    {st with players=updated_player_list}


(* [add_resources player n r] adds [n] resources [r] for player [player] *)
let add_resources player n = function
  | Lumber -> {player with lumber = player.lumber + n}
  | Wool   -> {player with wool = player.wool + n}
  | Grain  -> {player with grain = player.grain + n}
  | Brick  -> {player with brick = player.brick + n}
  | Ore    -> {player with ore = player.ore + n}
  | Null   -> player

(* [player_ok p] checks whether a player has enough resources for trade
 * raises: Failure when resouces aren't enough *)
let player_ok p =
  if p.lumber < 0 || p.wool < 0 || p.grain < 0 || p.brick < 0 || p.ore < 0
  then invalid_arg "Not enough resources"
  else p

(* [ports_of_player_helper st lst] returns ports for player at state [st] *)
let ports_of_player_helper st indices_list=
  let ports_temp =
    List.fold_left
      (
        fun acc element ->
          if List.mem (fst element.neighbors) indices_list
            || List.mem (snd element.neighbors) indices_list
          then element::acc
          else acc
      ) [] st.canvas.ports
  in (List.sort_uniq Pervasives.compare ports_temp)

(* [ports_of_player st color] returns ports for player with color [color]
 * at state [st] *)
let ports_of_player st color =
  let open Tile in
  let indices =
    List.fold_left (fun acc t -> acc @ List.fold_left (fun lst (i, (c, _)) ->
        if c = color then i :: lst else lst) [] t.buildings) [] st.canvas.tiles
  in ports_of_player_helper st indices

(* [ports_of_player_with_specific_resource st color rs] returns ports for
 * player with color [color] and resource [rs] at state [st] *)
let ports_of_player_with_specific_resource st color rs=
  let ports_belong_to_player = ports_of_player st color in
  List.fold_left (fun acc x -> if x.resource = rs then x::acc else acc)
    [] ports_belong_to_player

(* [ports_of_player_with_specific_resource_with_best_rate st color rs]
 * returns ports for player with color [color] and resource [rs]
 * at state [st] *)
let ports_of_player_with_specific_resource_with_best_rate st color rs=
  let ports_of_player_with_resource_wanted =
    ports_of_player_with_specific_resource st color rs
  in
  List.fold_left (fun acc x -> if x.rate < acc.rate then x else acc)
    (List.hd ports_of_player_with_resource_wanted)
    ports_of_player_with_resource_wanted

(* [trade_ok st p r1 r2] returns whether a trade can be valid *)
let trade_ok st p (rs, n) (rs', n') =
  if n / n' >= 4 then true
  else
    let best_port =
      (ports_of_player_with_specific_resource_with_best_rate st p.color rs) in
    if n / n' >= best_port.rate then true else false

(* [remove_resources player n r] removes [n] resource [r] for player [player]*)
let remove_resources player n r = add_resources player (-n) r |> player_ok

(* [indexof lst element] returns the index of element [element] in list [lst]*)
let rec indexof lst element=
  match lst with
  | [] -> raise (Failure "the element is not in the list")
  | h::t  -> if h=element then 0 else 1 + indexof t element

let trade_with_bank st to_remove to_add cl =
  let player = List.find (fun p -> p.color = cl) st.players in
  let length_of_resource_pass_trade_ok =
    List.fold_left (fun acc x ->
        if trade_ok st player x (List.nth to_add (indexof to_remove x)) then
          1 + acc else acc ) 0 to_remove
  in
  if length_of_resource_pass_trade_ok = List.length to_remove then
    (
      let player = List.fold_left (fun acc (r, n) -> remove_resources acc n r)
          player to_remove in
      let player = List.fold_left (fun acc (r, n) -> add_resources acc n r)
          player to_add in
      let players = List.map (fun p -> if p.color = cl then player else p)
          st.players in
      {st with players }
    )
  else
    raise (Failure "the trade with bank is not valid")

let trade_with_port st to_remove to_add cl=
  let player = List.find (fun p -> p.color = cl) st.players in
  let length_of_resource_pass_trade_ok =
    List.fold_left (fun acc x ->
        if trade_ok st player x (List.nth to_add (indexof to_remove x)) then 1 + acc
        else acc) 0 to_remove
  in
  if length_of_resource_pass_trade_ok = List.length to_remove then
    (
      let player =
        List.fold_left (fun acc (r, n) -> remove_resources acc n r)
          player to_remove in
      let player =
        List.fold_left (fun acc (r, n) -> add_resources acc n r) player to_add in
      let players =
        List.map (fun p -> if p.color = cl then player else p) st.players in
      { st with players }
    )
  else
    raise (Failure "the trade with port is not valid")

(* [check_whether_trade_is_ok_for_one_player st r a cl] checks whether a trade
 * is valid for player at state [st] *)
let check_whether_trade_is_ok_for_one_player st to_remove to_add cl=
  let player = List.find (fun p -> p.color = cl) st.players in
  let length_of_resource_pass_trade_ok =
    List.fold_left (fun acc x ->
        if trade_ok st player x (List.nth to_add (indexof to_remove x))
        then 1 + acc else acc ) 0 to_remove
  in
  if length_of_resource_pass_trade_ok = List.length to_remove then true else false

let trade_with_player st to_remove to_add cl =
  let condition_one =
    check_whether_trade_is_ok_for_one_player st to_remove to_add st.turn in
  let condition_two =
    check_whether_trade_is_ok_for_one_player st to_add to_remove cl in
  if condition_one && condition_two then
    let st' = trade_with_bank st to_remove to_add st.turn in
    trade_with_bank st' to_add to_remove cl
  else
    raise (Failure "the trade with other player is not valid")

let play_monopoly st rs =
  let steal (lst, n) p =
    if p.color = st.turn then
      p :: lst, n
    else
      let m = num_resources p rs in
      (remove_resources p m rs) :: lst, n + m
  in
  let result = List.fold_left steal ([], 0) st.players in
  let player = List.find (fun p -> p.color = st.turn) st.players in
  let player = add_resources player (snd result) rs in
  let player = { player with monopoly = player.monopoly - 1 } in
  let players =
    List.map (fun p -> if p.color = st.turn then player else p) (fst result) in
  {st with players}

let play_year_of_plenty st r1 r2 =
  let player = List.find (fun p -> p.color = st.turn) st.players in
  let player' = add_resources (add_resources player 1 r1) 1 r2 in
  let player'' = {player' with year_of_plenty = player'.year_of_plenty - 1 } in
  let players =
    List.map (fun p -> if p.color = st.turn then player'' else p) st.players in
  {st with players}

let do_player st cmd clr_opt =
  match cmd with
  | Setup (i, rd) -> init_build_road rd st.turn (init_build_settlement i st.turn st)
  | BuildSettlement i -> build_settlement i st st.turn
  | BuildCity i -> build_city i st st.turn
  | BuildRoad rd -> build_road rd st st.turn
  | BuyCard -> buy_devcard st.turn st
  | Knight i -> play_knight st st.turn i
  | RoadBuilding (rd0, rd1) -> failwith "TODO: Fix road building."
  | YearOfPlenty (rs0, rs1) -> play_year_of_plenty st rs0 rs1
  | Monopoly rs -> play_monopoly st rs
  | Robber i -> play_robber st st.turn i
  | DomesticTrade (lst0, lst1) ->
    begin
      match clr_opt with
      | None -> invalid_arg "Requires a color."
      | Some clr -> trade_with_player st lst0 lst1 clr
    end
  | MaritimeTrade (p0, p1) ->
    begin
      match clr_opt with
      | None -> invalid_arg "Requires a color."
      | Some clr ->
        begin
          match trade_with_port st [p0] [p1] st.turn with
          | exception (Failure _) -> trade_with_bank st [p0] [p1] st.turn
          | stx -> stx
        end
    end
  | Discard lst ->
    begin
      match clr_opt with
      | None -> invalid_arg "Requires a color."
      | Some clr -> discard_resource clr st lst
    end
  | EndTurn -> end_turn st
  | Accept b -> st
  | Quit -> st
  | Invalid -> st

let do_ai st = failwith "TODO"
