open Elements
open Player

type canvas = {
  tiles: Tile.tile list;
  ports: Trade.port list
}

type state = {
  robber: int;
  deck: DevCard.devcard list;
  players: Player.player list;
  canvas : canvas
}

let rec roll () =
  let i = 2 + Random.int 11 in
  if i <> 7 then i else roll ()

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
    ports = []
  }

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
    let res = List.fold_left (fun acc x -> acc && help tile_lst x = White) true ind_lst in
    if res = false then false
    else
      let rec help' lst num2 =
        match lst with
        | [] -> White
        | h::t ->
          if List.mem_assoc (num, num2) h.roads
          then List.assoc (num, num2) h.roads
          else if List.mem_assoc (num2, num) h.roads
          then List.assoc (num2, num) h.roads
          else help' t num2
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
  if res' = true then true else
    List.fold_left (fun acc x -> acc || help' tile_lst i0 x = color) false ind_lst_i0
    || List.fold_left (fun acc x -> acc || help' tile_lst i1 x = color) false ind_lst_i1

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

let play_road_build st color (i0, i1) =
  let open Tile in
  let player = List.filter (fun x -> x.color = color) st.players |> List.hd in
  if player.road_of_Building < 1 then
    failwith "No Road Building Card"
  else
    if check_build_road (i0, i1) st color = false then
      failwith "Cannot Build Road"
  else
    let new_tiles =
      List.map (fun t -> if List.mem i0 t.indices && List.mem i1 t.indices then
                   {t with roads=((i0, i1), color)::t.roads} else t) st.canvas.tiles in
    let new_players =
      List.map (fun x -> if x <> player then x
                 else {player with road_of_Building = player.road_of_Building-1}) st.players in
    {st with canvas = {tiles = new_tiles; ports=st.canvas.ports};
             players = new_players}

let play_monopoly st = failwith "TODO"

let play_year_of_plenty st = failwith "TODO"

let play_victory st color =
  let open Player in
  let f e =
    if e.color = color then {e with score = e.score + 2}
    else e in {st with players = List.map f st.players}

let play_knight st = failwith "TODO"

let play_devcard card st = failwith "TODO"

(* need [1 grain, 1 ore, 1 wool] *)
let buy_devcard color st =
  let open Player in
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
                   | Road_of_Building -> {x with road_of_Building = x.road_of_Building+1;
                                                 grain = x.grain-1;
                                                 ore = x.ore-1;
                                                 wool = x.wool-1}
                   | Year_of_Plenty -> {x with year_of_Plenty = x.year_of_Plenty+1;
                                               grain = x.grain-1;
                                               ore = x.ore-1;
                                               wool = x.wool-1}
                   | Monopoly -> {x with monopoly = x.monopoly+1;
                                         grain = x.grain-1;
                                         ore = x.ore-1;
                                         wool = x.wool-1}
                   | Victory_Point -> {x with victory_Point = x.victory_Point+1;
                                              grain = x.grain-1;
                                              ore = x.ore-1;
                                              wool = x.wool-1}) lst
    in
    let st' = {st with deck = rest; players = updated_players color st.players} in
    match card with
    | Victory_Point -> play_victory st' color
    | _ -> st'

let init_phase st = failwith "TODO"

let play_robber st = failwith "TODO"

(* check: 1. whether player's number of settlements < 5
          2. resource is enough [1 lumber, 1 brick, 1 ore, i wool]
          3. whether check_build_settlements returns true *)
let can_build_settlements ind st color =
  let open Tile in
  let num_settlements = st.canvas.tiles
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
    if player.lumber < 1 || player.brick < 1 || player.ore < 1 || player.wool < 1 then
      failwith "You do not have enough resource to build a settlement"
    else true

(* check: 1. whether players number of roads < 15
          2. resource is enough [1 lumber, 1 brick]
          3. whether check_build_road returns true *)
let can_build_road (i0, i1) st color =
  let open Tile in
  let num_roads = st.canvas.tiles
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

(* check: 1. whether players number of cities < 4
          2. resource is enough [3 grains, 2 ores]
          3. whether check_build_cities returns true *)
let can_build_city ind st color =
  let open Tile in
  let num_cities = st.canvas.tiles
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
  let new_players = st.players
                    |> List.map (fun x -> if x.color <> color then x
                                  else {x with lumber = x.lumber-1;
                                               brick = x.brick-1;
                                               ore = x.ore-1;
                                               wool = x.wool-1})
  in
  let new_tiles = st.canvas.tiles
                  |> List.map (fun x -> if List.mem ind x.indices = false then x
                                else {x with buildings = (ind, (color, 1))::x.buildings})
  in {st with players = new_players; canvas = {tiles = new_tiles; ports = st.canvas.ports}}

let build_road (i0, i1) st color =
  let open Tile in
  let _ = can_build_road (i0, i1) st color in
  let new_players = st.players
                    |> List.map (fun x -> if x.color <> color then x
                                  else {x with lumber = x.lumber-1;
                                               brick = x.brick-1})
  in
  let new_tiles = st.canvas.tiles
                  |> List.map (fun x -> if List.mem i0 x.indices = false
                                        || List.mem i1 x.indices = false then x
                                else {x with roads = ((i0, i1), color)::x.roads})
  in {st with players = new_players; canvas = {tiles = new_tiles; ports = st.canvas.ports}}

let build_city ind st color =
  let open Tile in
  let _ = can_build_city ind st color in
  let new_players = st.players
                    |> List.map (fun x -> if x.color <> color then x
                                  else {x with grain = x.grain-3;
                                               ore = x.ore-2})
  in
  let new_tiles =
    st.canvas.tiles
    |> List.map (fun x -> if List.mem ind x.indices = false then x
                  else {x with buildings = List.map (fun (a, (b, c)) ->
                      if a = ind then (a, (b, 2)) else (1, (b, c))) x.buildings})
  in {st with players = new_players; canvas = {tiles = new_tiles; ports = st.canvas.ports}}

let check_robber tile st =
  let robber_ind = st.robber in
  let tile_at_ind = List.nth st.canvas.tiles robber_ind in
  tile_at_ind = tile

let generate_resource st num =
  let open Tile in
  let open Player in
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
        | h::t -> if h.color = col then
                    let newp =
                    begin
                      match resource with
                      | Lumber -> {h with lumber = h.lumber + mul}
                      | Wool -> {h with wool = h.wool + mul}
                      | Grain -> {h with grain = h.grain + mul}
                      | Brick -> {h with brick = h.brick + mul}
                      | Ore -> {h with ore = h.ore + mul}
                    end
                    in new_players t (newp::acc)
          else new_players t (h::acc)
        in help {st with players = new_players st.players []} t
      end
  in help st info

let count_resource_card player resource =
  match resource with
  | Lumber -> player.lumber
  | Wool -> player.wool
  | Grain -> player.grain
  | Brick -> player.brick
  | Ore -> player.ore

(*helper function returns the corresponding player for the input color
  and game state*)
let rec check_player_color st cl=
  let rec help_check_color pl_lst' cl'=
    match pl_lst' with
    | [] -> failwith "impossible"
    | h::t ->
      if h.color = cl' then h else help_check_color t cl'
  in help_check_color st.players cl

(*helper function for trade_with_bank, only remove resource cards*)
let trade_with_bank_helper st rs color =
  let player_now = check_player_color st color
  in let number_of_resource_card = count_resource_card player_now rs
  in  if number_of_resource_card < 4 then
    raise (Failure("not enough resource cards"))
  else match rs with
    | Lumber -> {player_now with lumber=player_now.lumber - 4}
    | Wool ->  {player_now with wool=player_now.wool - 4}
    | Grain ->  {player_now with grain=player_now.grain - 4}
    | Brick ->  {player_now with brick=player_now.brick - 4}
    | Ore ->  {player_now with ore=player_now.ore - 4}

(*helper function for trade_with_bank, which returns the updated player*)
let trade_with_bank_helper' st rs rs' color =
  let updated = trade_with_bank_helper st rs color in
  match rs' with
  | Lumber -> {updated with lumber=updated.lumber + 1}
  | Wool ->  {updated with wool=updated.wool + 1}
  | Grain ->  {updated with grain=updated.grain + 1}
  | Brick ->  {updated with brick=updated.brick + 1}
  | Ore ->  {updated with ore=updated.ore + 1}

let trade_with_bank st rs rs' cl=
  let updated_player=trade_with_bank_helper' st rs rs' cl in
  let updated_list=List.filter (fun s -> s.color<>cl) st.players in
  {st with players=updated_player::updated_list}


let do_player st = failwith "TODO"

let do_ai st = failwith "TODO"
