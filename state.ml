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
      { indices = [0; 4; 8; 12; 7; 3];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center +. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [1; 5; 9; 13; 8; 4];
        dice = roll ();
        resource = random_resource ();
        center = fst center, snd center +. 3. *. length ;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [2; 6; 10; 14; 9; 5];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center +. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [7; 12; 17; 22; 16; 11];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [8; 13; 18; 23; 17; 12];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [9; 14; 19; 24; 18; 13];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [10; 15; 20; 25; 19; 14];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [16; 22; 28; 33; 27; 21];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 4. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [17; 23; 29; 34; 28; 22];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [18; 24; 30; 35; 29; 23];
        dice = roll ();
        resource = random_resource ();
        center = fst center, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [19; 25; 31; 36; 30; 24];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [20; 26; 32; 37; 31; 25];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 4. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [28; 34; 39; 43; 38; 33];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [29; 35; 40; 44; 39; 34];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [30; 36; 41; 45; 40; 35];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [31; 37; 42; 46; 41; 36];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [39; 44; 48; 51; 47; 43];
        dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center -. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [40; 45; 49; 52; 48; 44];
        dice = roll ();
        resource = random_resource ();
        center = fst center, snd center -. 3. *. length ;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [41; 46; 50; 53; 49; 45];
        dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center -. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
    ];
    ports = []
  }

let check_build_building coord st = failwith "TODO"

let check_build_road coord st = failwith "TODO"

let fetch_tiles num tiles =
  List.filter (fun x -> x.Tile.dice = num) tiles

let update_dev_list lst x = failwith "TODO"

let play_road_build st color road =
  failwith "TODO"

let play_monopoly st = failwith "TODO"

let play_year_of_plenty st = failwith "TODO"

let play_victory st color =
  let open Player in
  let f e =
    if e.color = color then {e with score = e.score + 2}
    else e in {st with players = List.map f st.players}

let play_knight st = failwith "TODO"

let play_devcard card st = failwith "TODO"

let move_robber st = failwith "TODO"

let build_building st = failwith "TODO"

let build_road st = failwith "TODO"

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

let trade_with_bank st rs rs' color =
  let updated = trade_with_bank_helper st rs color in
  match rs' with
  | Lumber -> {updated with lumber=updated.lumber + 1}
  | Wool ->  {updated with wool=updated.wool + 1}
  | Grain ->  {updated with grain=updated.grain + 1}
  | Brick ->  {updated with brick=updated.brick + 1}
  | Ore ->  {updated with ore=updated.ore + 1}


let do_player st = failwith "TODO"

let do_ai st = failwith "TODO"
