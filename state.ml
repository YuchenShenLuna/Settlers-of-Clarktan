open Elements
open Player
open Command
open Tile

(*****************************************************************************
 *                                DEFINITIONS                                *
 *****************************************************************************)

type port = {
  neighbors : int * int;
  demand : resource option;
  rate : int
}

type canvas = {
  tiles : Tile.tile list;
  ports : port list
}

type state = {
  robber : int;
  deck: card list;
  turn : color;
  players : Player.player list;
  canvas : canvas
}

(*****************************************************************************
 *                               INITIAL PHASE                               *
 *****************************************************************************)

let init_canvas () =
  let rec roll () =
    let i = 2 + Random.int 11 in
    if i <> 7 then Some i else roll ()
  in
  let random_resource () =
    match Random.int 5 with
    | 0 -> Some Lumber
    | 1 -> Some Wool
    | 2 -> Some Grain
    | 3 -> Some Brick
    | _ -> Some Ore
  in
  let tinfo =
    let rec helper f n acc =
      if n = 0 then acc else f () :: acc |>  helper f (n - 1)
    in
    let l0 = [ Some 2; Some 3; Some 4; Some 5; Some 6;
               Some 8; Some 9; Some 10; Some 11; Some 12 ]
             |> shuffle |> helper roll 8 in
    let l1 = [ Some Lumber; Some Wool; Some Grain; Some Brick; Some Ore;
               Some Lumber; Some Wool; Some Grain; Some Brick; Some Ore;
               Some Lumber; Some Wool; Some Grain; Some Brick; Some Ore ]
             |> shuffle |> helper random_resource 3 in
    (None, None) :: (List.combine l0 l1) |> shuffle
  in
  let pinfo =
    [ Some Lumber, 2; Some Wool, 2; Some Grain, 2; Some Brick, 2; Some Ore, 2;
      None, 3; None, 3; None, 3; None, 3 ] |> shuffle
  in
  let center = 500., 375. in
  let length = 50. in
  let apothem =  length *. sqrt 3. /. 2. in
  {
  tiles = [
    { indices = [3; 4; 15; 14; 13; 2];
      dice = List.nth tinfo 0 |> fst;
      resource = List.nth tinfo 0 |> snd;
      center = fst center -. 2. *. apothem, snd center +. 3. *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [5; 6; 17; 16; 15; 4];
      dice = List.nth tinfo 1 |> fst;
      resource = List.nth tinfo 1 |> snd;
      center = fst center, snd center +. 3. *. length ;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [7; 8; 19; 18; 17; 6];
      dice = List.nth tinfo 2 |> fst;
      resource = List.nth tinfo 2 |> snd;
      center = fst center +. 2. *. apothem, snd center +. 3. *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [13; 14; 25; 24; 23; 12];
      dice = List.nth tinfo 3 |> fst;
      resource = List.nth tinfo 3 |> snd;
      center = fst center -. 3. *. apothem, snd center +. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [15; 16; 27; 26; 25; 14];
      dice = List.nth tinfo 4 |> fst;
      resource = List.nth tinfo 4 |> snd;
      center = fst center -. apothem, snd center +. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [17; 18; 29; 28; 27; 16];
      dice = List.nth tinfo 5 |> fst;
      resource = List.nth tinfo 5 |> snd;
      center = fst center +. apothem, snd center +. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [19; 20; 31; 30; 29; 18];
      dice = List.nth tinfo 6 |> fst;
      resource = List.nth tinfo 6 |> snd;
      center = fst center +. 3. *. apothem, snd center +. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [23; 24; 35; 34; 33; 22];
      dice = List.nth tinfo 7 |> fst;
      resource = List.nth tinfo 7 |> snd;
      center = fst center -. 4. *. apothem, snd center;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [25; 26; 37; 36; 35; 24];
      dice = List.nth tinfo 8 |> fst;
      resource = List.nth tinfo 8 |> snd;
      center = fst center -. 2. *. apothem, snd center;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [27; 28; 39; 38; 37; 26];
      dice = List.nth tinfo 9 |> fst;
      resource = List.nth tinfo 9 |> snd;
      center = fst center, snd center;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [29; 30; 41; 40; 39; 28];
      dice = List.nth tinfo 10 |> fst;
      resource = List.nth tinfo 10 |> snd;
      center = fst center +. 2. *. apothem, snd center;
      edge = length;
      buildings = [];
      roads = []};
    { indices = [31; 32; 43; 42; 41; 30];
      dice = List.nth tinfo 11 |> fst;
      resource = List.nth tinfo 11 |> snd;
      center = fst center +. 4. *. apothem, snd center;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [35; 36; 47; 46; 45; 34];
      dice = List.nth tinfo 12 |> fst;
      resource = List.nth tinfo 12 |> snd;
      center = fst center -. 3. *. apothem, snd center -. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [37; 38; 49; 48; 47; 36];
      dice = List.nth tinfo 13 |> fst;
      resource = List.nth tinfo 13 |> snd;
      center = fst center -. apothem, snd center -. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [39; 40; 51; 50; 49; 38];
      dice = List.nth tinfo 14 |> fst;
      resource = List.nth tinfo 14 |> snd;
      center = fst center +. apothem, snd center -. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [41; 42; 53; 52; 51; 40];
      dice = List.nth tinfo 15 |> fst;
      resource = List.nth tinfo 15 |> snd;
      center = fst center +. 3. *. apothem, snd center -. 1.5 *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [47; 48; 59; 58; 57; 46];
      dice = List.nth tinfo 16 |> fst;
      resource = List.nth tinfo 16 |> snd;
      center = fst center -. 2. *. apothem, snd center -. 3. *. length;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [49; 50; 61; 60; 59; 48];
      dice = List.nth tinfo 17 |> fst;
      resource = List.nth tinfo 17 |> snd;
      center = fst center, snd center -. 3. *. length ;
      edge = length;
      buildings = [];
      roads = [] };
    { indices = [51; 52; 63; 62; 61; 50];
      dice = List.nth tinfo 18 |> fst;
      resource = List.nth tinfo 18 |> snd;
      center = fst center +. 2. *. apothem, snd center -. 3. *. length;
      edge = length;
      buildings = [];
      roads = [] }
  ];
  ports = [
    { neighbors = 2, 3;
      demand = List.nth pinfo 0 |> fst;
      rate = List.nth pinfo 0 |> snd };
    { neighbors = 5, 6;
      demand = List.nth pinfo 1 |> fst;
      rate = List.nth pinfo 1 |> snd };
    { neighbors = 12, 23;
      demand = List.nth pinfo 2 |> fst;
      rate = List.nth pinfo 2 |> snd };
    { neighbors = 19, 20;
      demand = List.nth pinfo 3 |> fst;
      rate = List.nth pinfo 3 |> snd };
    { neighbors = 32, 43;
      demand = List.nth pinfo 4 |> fst;
      rate = List.nth pinfo 4 |> snd };
    { neighbors = 34, 45;
      demand = List.nth pinfo 5 |> fst;
      rate = List.nth pinfo 5 |> snd };
    { neighbors = 52, 53;
      demand = List.nth pinfo 6 |> fst;
      rate = List.nth pinfo 6 |> snd };
    { neighbors = 57, 58;
      demand = List.nth pinfo 7 |> fst;
      rate = List.nth pinfo 7 |> snd };
    { neighbors = 60, 61;
      demand = List.nth pinfo 8 |> fst;
      rate = List.nth pinfo 8 |> snd };
  ]
}

let init_state () =
  let rec desert = function
    | [] -> raise Not_found
    | h :: t  -> if h.resource = None then 0 else 1 + desert t
  in
  let canvas = init_canvas () in
  let players = shuffle [ Player.init_player Red; Player.init_player Yellow;
                          Player.init_player Blue; Player.init_player Green ] in
  { robber = desert canvas.tiles;
    deck = shuffle [ Knight; VictoryPoint; Knight; RoadBuilding;YearOfPlenty;
                     Knight; RoadBuilding; Knight; Knight; VictoryPoint; Knight;
                     Knight; Monopoly; Knight; YearOfPlenty; Knight; Knight;
                     VictoryPoint; Knight; Knight; Knight; Monopoly;
                     VictoryPoint; Knight; VictoryPoint ];
    players;
    turn = (List.hd players).color;
    canvas }

(* [fetch_neighbors i] fetches the neighboring intersections of the
 * settlement with index [i]*)
let fetch_neighbors i =
  let fetch i =
    if i mod 2 = 1 then [i - 11; i - 1; i + 1]
    else [i + 11; i - 1; i + 1]
  in
  let lst = fetch i in
  let valid_index =
    [ 2; 3; 4; 5; 6; 7; 8; 12; 13; 14; 15; 16; 17; 18; 19; 20; 22; 23; 24; 25;
      26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43;
      45; 46; 47; 48; 49; 50; 51; 52; 53; 57; 58; 59; 60; 61; 62; 63 ]
  in
  List.filter (fun x -> List.mem x valid_index) lst

let check_initialize_build_settlement ind st =
  let neighbors = fetch_neighbors ind in
  let tiles = st.canvas.tiles in
  let rec help lst num =
    match lst with
    | [] -> None
    | h::t ->
      match List.assoc_opt num h.buildings with
      | None -> help t num
      | Some (color, _) -> Some color
  in
  if help tiles ind <> None then
    false
  else
    let res = List.fold_left
        (fun acc x -> acc && help tiles x = None) true neighbors in
    if res = false then false else true

let init_build_settlement ind color st =
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

let check_initialize_build_road (i0, i1) st color =
  let tiles = st.canvas.tiles in
  let rec help lst num =
    match lst with
    | [] -> None
    | h::t ->
      match List.assoc_opt num h.buildings with
      | None -> help t num
      | Some (color, _) -> Some color
  in
  if help tiles i0 <> Some color && help tiles i1 <> Some color then
    false
  else
    let rec help' lst x y =
      match lst with
      | [] -> None
      | h::t ->
        if List.mem_assoc (x, y) h.roads then
          List.assoc_opt (x, y) h.roads
        else if List.mem_assoc (y, x) h.roads then
          List.assoc_opt (y, x) h.roads
        else
          help' t x y
    in
    let res = help' tiles i0 i1 = None in
    if res = false then false
    else
      let i0_neighbors = fetch_neighbors i0 in
      let i1_neighbors = fetch_neighbors i1 in
      let has_no_road i =
        List.fold_left (fun acc x -> acc && (help' tiles i0 x = None)) true i
      in
      (help tiles i0 = Some color && has_no_road i0_neighbors) ||
      (help tiles i1 = Some color && has_no_road i1_neighbors)

let init_build_road (i0, i1) color st =
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
              wool = x.wool + count (Some Wool);
              brick = x.brick + count (Some Brick);
              lumber = x.lumber + count (Some Lumber);
              ore = x.ore + count (Some Ore);
              grain = x.grain + count (Some Grain)})
  in {st with players = new_players}

(*****************************************************************************
 *                                   BUILD                                   *
 *****************************************************************************)

let check_build_settlement n st color =
  let ind_lst = n :: fetch_neighbors n in
  let tile_lst = st.canvas.tiles in
  let rec help lst n =
    match lst with
    | [] -> None
    | h :: t ->
      match List.assoc_opt n h.buildings with
      | None -> help t n
      | Some (color, _) -> Some color
  in
  List.fold_left (fun acc x -> acc && help tile_lst x = None) true ind_lst
  &&
  let rec help' lst n2 =
    match lst with
    | [] -> None
    | h :: t ->
      if List.mem_assoc (n, n2) h.roads then List.assoc_opt (n, n2) h.roads
      else if List.mem_assoc (n2, n) h.roads then List.assoc_opt (n2, n) h.roads
      else help' t n2
  in
  List.fold_left (fun acc x -> acc || help' tile_lst x = Some color) false ind_lst

let can_build_settlement color ind st =
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
  else if check_build_settlement ind st color = false then
    failwith "You cannot build settlement at this place"
  else
    let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
    if player.lumber < 1 || player.brick < 1
       || player.ore < 1 || player.wool < 1 then
      failwith "You do not have enough resource to build a settlement"
    else true

let build_settlement ind st =
  let _ = can_build_settlement st.turn ind st in
  let new_players =
    st.players
    |> List.map (fun x -> if x.color <> st.turn then x
                  else {x with lumber = x.lumber-1;
                               brick = x.brick-1;
                               ore = x.ore-1;
                               wool = x.wool-1})
  in
  let new_tiles =
    st.canvas.tiles
    |> List.map (fun x -> if List.mem ind x.indices = false then x
                  else {x with buildings = (ind, (st.turn, 1))::x.buildings})
  in
  {st with players = new_players;
           canvas = {tiles = new_tiles;
                     ports = st.canvas.ports}}

let check_build_road (i0, i1) st color =
  let ind_lst_i0 = List.filter (fun x -> x <> i0 && x <> i1)
      (fetch_neighbors i0) in
  let ind_lst_i1 = List.filter (fun x -> x <> i0 && x <> i1)
      (fetch_neighbors i1) in
  let tile_lst = st.canvas.tiles in
  let rec help' lst x y =
    match lst with
    | [] -> None
    | h::t ->
      if List.mem_assoc (x, y) h.roads
      then List.assoc_opt (x, y) h.roads
      else if List.mem_assoc (y, x) h.roads
      then List.assoc_opt (y, x) h.roads
      else help' t x y
  in
  let res = help' tile_lst i0 i1 = None in
  if res = false then false
  else
  let rec help lst num =
    match lst with
    | [] -> None
    | h::t ->
      match List.assoc_opt num h.buildings with
      | None -> help t num
      | Some (color, _) -> Some color
  in
  let res' = help tile_lst i0 = Some color || help tile_lst i1 = Some color in
  if res' then
    true
  else
    List.fold_left (fun acc x ->
        acc || help' tile_lst i0 x = Some color) false ind_lst_i0
    || List.fold_left (fun acc x ->
        acc || help' tile_lst i1 x = Some color) false ind_lst_i1

let can_build_road color (i0, i1) st =
  let num_roads =
    st.canvas.tiles
    |> List.map (fun x -> x.roads)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.filter (fun (x, y) -> y = color)
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

let build_road (i0, i1) st =
  let _ = can_build_road st.turn (i0, i1) st  in
  let new_players =
    st.players
    |> List.map (fun x -> if x.color <> st.turn then x
                  else {x with lumber = x.lumber-1;
                               brick = x.brick-1})
  in
  let new_tiles =
    st.canvas.tiles
    |> List.map (fun x -> if List.mem i0 x.indices = false
                          || List.mem i1 x.indices = false then x
                          else {x with roads = ((i0, i1), st.turn)::x.roads})
  in
  {st with players = new_players;
           canvas = {tiles = new_tiles;
                     ports = st.canvas.ports}}

let check_build_cities num st color =
  let tile_lst = st.canvas.tiles in
  let rec help lst num =
    match lst with
    | [] -> None, 1
    | h::t ->
      match List.assoc_opt num h.buildings with
      | None -> help t num
      | Some (color, ty) -> (Some color, ty)
  in
  help tile_lst num = (Some color, 1)

let can_build_city color ind st =
  let num_cities =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.filter (fun (x, (y, z)) -> y = color && z=2)
    |> List.length
  in
  if num_cities >= 5 then
    failwith "You have build the maximum number of cities possible"
  else if check_build_cities ind st color = false then
    failwith "You cannot build city at this place"
  else
    let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
    if player.grain < 2 || player.ore < 3 then
      failwith "You do not have enough resource to build a city"
    else true

let build_city ind st =
  let _ = can_build_city st.turn ind st in
  let new_players =
    st.players |>
    List.map (fun x -> if x.color <> st.turn then x
               else {x with grain = x.grain-2; ore = x.ore-3})
  in
  let new_tiles =
    st.canvas.tiles
    |> List.map (
      fun x ->
        if List.mem ind x.indices = false then x
        else { x with buildings = List.map (fun (a, (b, c)) ->
            if a = ind then (a, (b, 2)) else (a, (b, c))) x.buildings }
      )
  in
  {st with players = new_players;
           canvas = {tiles = new_tiles; ports = st.canvas.ports}}

let buy_card st =
  let player = List.hd (List.filter (fun x -> x.color=st.turn) st.players) in
  if player.grain < 1 || player.ore < 1 || player.wool < 1 then
    failwith "Not enough resource to buy development card"
  else if List.length st.deck < 1 then
    failwith "Development cards sold out"
  else
    let card_lst = st.deck in
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
    {st with deck = rest; players = updated_players st.turn st.players}

(*****************************************************************************
 *                                   TRADE                                   *
 *****************************************************************************)

let get_player color s = List.find (fun p -> p.color = color) s.players

let player_ok color s =
  let p = get_player color s in
  p.lumber < 0 || p.wool < 0 || p.grain < 0 || p.brick < 0 || p.ore < 0

let add_resources to_add color s =
  let rec add player = function
    | [] -> player
    | (r, n) :: t ->
      match r with
      | Lumber -> add { player with lumber = player.lumber + n } t
      | Wool   -> add { player with wool = player.wool + n } t
      | Grain  -> add { player with grain = player.grain + n } t
      | Brick  -> add { player with brick = player.brick + n } t
      | Ore    -> add { player with ore = player.ore + n } t
  in
  let players =
    List.map (
      fun x -> if x.color = color then add (get_player color s) to_add else x
    ) s.players in
  { s with players }

let remove_resources to_remove color s =
  let to_add = List.map (fun (r, n) -> r, -n) to_remove in
  add_resources to_add color s

let best_rate resource color s =
  let is_settled intersection =
    List.fold_left (
      fun acc t ->
        acc || List.fold_left (
          fun acc (i, (c, _)) ->
            acc || intersection = i && c = color
        ) false t.buildings
    ) false s.canvas.tiles
  in
  let ports =
    List.fold_left (
      fun acc x ->
        if x.demand = Some resource || x.demand = None
        && (is_settled (fst x.neighbors) || is_settled (snd x.neighbors))
        then x :: acc
        else acc
    ) [] s.canvas.ports
  in
  List.fold_left (fun acc x -> if x.rate < acc then x.rate else acc) 4 ports

let trade_ok to_remove to_add partner_opt s =
  match partner_opt with
  | None ->
    List.fold_left (
      fun acc (r, n) -> acc + n / best_rate r s.turn s
    ) 0 to_remove
    >= List.fold_left (fun acc (_, n) -> acc + n) 0 to_add
  | Some partner ->
    s |> remove_resources to_remove s.turn |> player_ok s.turn
    && s |> remove_resources to_add partner |> player_ok partner
    && s |> add_resources to_add s.turn |> player_ok s.turn
    && s |> add_resources to_remove partner |> player_ok partner

let print_resources =
  List.iter (
    fun (r, n) ->
      let msg = string_of_resource r ^ " * " ^ string_of_int n in
      print_endline msg
  )

let domestic to_remove to_add partner s =
  if trade_ok to_remove to_add (Some partner) s then
    s |> remove_resources to_remove s.turn
    |> remove_resources to_add partner
    |> add_resources to_add s.turn
    |> add_resources to_remove partner
  else
    let () = print_endline "REMOVE:" in
    let () = print_resources to_remove in
    let () = print_endline "ADD:" in
    let () = print_resources to_add in
    failwith "Bad trade!"

let maritime to_remove to_add s =
  if trade_ok to_remove to_add None s then
    s |> remove_resources to_remove s.turn
    |> add_resources to_add s.turn
  else
    let () = print_endline "REMOVE:" in
    let () = print_resources to_remove in
    let () = print_endline "ADD:" in
    let () = print_resources to_add in
    failwith "Bad trade!"

(*****************************************************************************
 *                          PLAY A DEVELOPMENT CARD                          *
 *****************************************************************************)

(* [num_resource player] returns the number of resources for player [player] *)
let num_resource color resource st =
  let player = get_player color st in
  match resource with
  | Lumber -> player.lumber
  | Wool   -> player.wool
  | Grain  -> player.grain
  | Brick  -> player.brick
  | Ore    -> player.ore

let num_all_resources color st =
  num_resource color Lumber st +
  num_resource color Wool st +
  num_resource color Grain st +
  num_resource color Brick st +
  num_resource color Ore st

let play_card card color s =
  let hand player = function
    | VictoryPoint -> player
    | RoadBuilding -> { player with road_building = player.road_building - 1 }
    | Monopoly -> { player with monopoly = player.monopoly - 1 }
    | YearOfPlenty -> { player with year_of_plenty = player.year_of_plenty - 1 }
    | Knight -> { player with knight = player.knight - 1;
                              knights_activated = player.knights_activated + 1 }
  in
  let players =
    List.map (
      fun x -> if x.color = color then hand (get_player color s) card else x
    ) s.players in
  { s with players }

let play_robber ind st =
  let pos_stealees =
    (List.nth st.canvas.tiles ind).buildings
    |> List.map (fun (_, (col, _)) -> col)
    |> List.sort_uniq compare
    |> List.filter (fun x -> num_all_resources x st > 0) in
  let shuffle lst =
    let i = Random.int (List.length lst) in
    List.nth lst i
  in
  let stealee_color = shuffle pos_stealees in
  let stealee =
    List.hd (List.filter (fun x -> x.color = stealee_color) st.players) in
  let pos_wool = if stealee.wool > 0 then [Some Wool] else [] in
  let pos_lumber = if stealee.lumber > 0 then [Some Lumber] else [] in
  let pos_brick = if stealee.wool > 0 then [Some Brick] else [] in
  let pos_ore = if stealee.lumber > 0 then [Some Ore] else [] in
  let pos_grain = if stealee.wool > 0 then [Some Grain] else [] in
  let pos_resource = pos_wool @ pos_brick @ pos_lumber @ pos_grain @ pos_ore in
  let stolen_resource = shuffle pos_resource in
  let new_players =
    st.players
    |> List.map (fun x -> if x.color = st.turn then
                    begin
                      match stolen_resource with
                      | Some Wool -> {x with wool = x.wool+1}
                      | Some Brick -> {x with brick = x.brick+1}
                      | Some Lumber -> {x with lumber = x.lumber+1}
                      | Some Ore -> {x with ore = x.ore+1}
                      | Some Grain -> {x with grain = x.grain+1}
                      | None -> x end
                  else if x.color = stealee_color then
                    begin
                      match stolen_resource with
                      | Some Wool -> {x with wool = x.wool-1}
                      | Some Brick -> {x with brick = x.brick-1}
                      | Some Lumber -> {x with lumber = x.lumber-1}
                      | Some Ore -> {x with ore = x.ore-1}
                      | Some Grain -> {x with grain = x.grain-1}
                      | None -> x end
                  else x)
  in {st with players = new_players;
              robber = ind}

let play_knight ind st =
  let st' = play_robber ind st in
  let new_players =
    List.map (fun x -> if x.color <> st.turn then x
               else
                 {x with knight = x.knight - 1;
                         knights_activated = x.knights_activated+1}) st'.players
  in {st' with players = new_players}

let play_road_build (i0, i1) (j0, j1) st =
  let player = List.filter (fun x -> x.color = st.turn) st.players |> List.hd in
  if player.road_building < 1 then
    failwith "No Road Building Card"
  else
  if check_build_road (i0, i1) st st.turn = false then
    failwith "Cannot Build Road at given first place"
  else
    let new_tiles =
      List.map (fun t -> if List.mem i0 t.indices && List.mem i1 t.indices then
                   {t with roads=((i0, i1), st.turn)::t.roads}
                 else t) st.canvas.tiles in
    let new_players =
      List.map (fun x -> if x <> player then x
                 else {player with road_building = player.road_building-1})
        st.players in
    let st' = {st with canvas = {tiles = new_tiles; ports=st.canvas.ports};
                       players = new_players}
    in
    if check_build_road (j0, j1) st' st.turn = false then
      failwith "Cannot Build Road at given second place"
    else
      let new_tiles' =
        List.map (fun t -> if List.mem i0 t.indices && List.mem i1 t.indices then
                     {t with roads=((i0, i1), st.turn)::t.roads}
                   else t) st'.canvas.tiles in
      {st' with canvas = {tiles = new_tiles'; ports=st.canvas.ports}}

let play_monopoly r s =
  List.fold_left (
    fun acc x ->
      if x.color = s.turn then acc
      else
        let to_steal = [ r, num_resource x.color r s ] in
        s |> remove_resources to_steal x.color |> add_resources to_steal s.turn
  ) s s.players

let play_year_of_plenty r1 r2 s =
  s |> add_resources [r1, 1; r2, 1] s.turn |> play_card YearOfPlenty s.turn

(*****************************************************************************
 *                                 RESOURCES                                 *
 *****************************************************************************)

let tiles_of_roll num st =
  List.filter (fun x -> x.Tile.dice = Some num) st.canvas.tiles

(* [check_robber tile st] checks whether a robber is at tile [tile] under
 * state [st] *)
let check_robber tile st =
  let robber_ind = st.robber in
  if robber_ind > 18 || robber_ind < 0 then true
  else List.nth st.canvas.tiles robber_ind = tile

let generate_resource num st =
  let tiles = tiles_of_roll num st in
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
        let rec new_players playerlst =
        match playerlst with
          | [] -> []
        | h::t ->
          if h.color = col then
            let newp =
            begin
              match resource with
              | Some Lumber -> {h with lumber = h.lumber + mul}
              | Some Wool -> {h with wool = h.wool + mul}
              | Some Grain -> {h with grain = h.grain + mul}
              | Some Brick -> {h with brick = h.brick + mul}
              | Some Ore -> {h with ore = h.ore + mul}
              | None -> h
            end
            in newp::(new_players t)
          else
            h::(new_players t)
        in help {st with players = new_players st.players} t
      end
  in help st info

let discard_resource color lst st =
  if num_all_resources color st < 7 then st
  else
    let num_in_lst = List.fold_left (fun acc (a, b) -> acc + b) 0 lst in
    if num_in_lst < num_all_resources color st then
      failwith "you need to discard more resources"
    else if num_in_lst > num_all_resources color st then
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

(*****************************************************************************
 *                               ACHIEVEMENT                                 *
 *****************************************************************************)

let get_player color st = List.find (fun p -> p.color = color) st.players

(* [fetch_roads_of_one_player st] fetches the roads at given state [st]*)
let fetch_roads_of_one_player color st =
  let cmp (s1, e1) (s2, e2) =
    if s1 < s2 then -1
    else if s1 > s2 then 1
    else if e1 < e2 then -1
    else if e1 > e2 then 1
    else 0
  in
  List.fold_left (fun acc t ->
      (t.indices
       |> List.mapi (fun i j ->
           let k = List.nth t.indices ((i + 1) mod 6) in
           if j < k then j, k else k, j)
       |> List.filter (fun (i, j) ->
           List.assoc_opt (i, j) t.roads = Some color
           || List.assoc_opt (j, i) t.roads = Some color)
      ) @ acc
    ) [] st.canvas.tiles |> List.sort_uniq cmp

(* [find_possible_owner_of_road_one_tile st rd_list road] finds all possible
 * owners of the given road [road] at state [st] *)
let rec find_possible_owner_of_road_one_tile st rd_list (s,e) =
  match rd_list with
  | [] -> None
  | h::t ->
    if (s,e) = fst h then
      Some (List.find (fun p -> p.color = snd h) st.players)
    else
      find_possible_owner_of_road_one_tile st t (s,e)

(* [find_owner_of_road st rd] finds the owner of the road [rd] at state [st] *)
let find_owner_of_road st rd =
  List.fold_left (fun acc x ->
      if find_possible_owner_of_road_one_tile st (x.roads) rd <> None then
        find_possible_owner_of_road_one_tile st x.roads rd
      else acc) (None) (st.canvas.tiles)

(* longest_road_helper returns the longest_road just for one player*)
let longest_road_length st cl=
  let roads = fetch_roads_of_one_player cl st in
  if roads = [] then
    0
  else
    let successors n e =
      List.map (fun (_, v) -> v) (List.filter (fun (u, _) -> n = u) e)
    in
    let dfs graph start  =
      let rec rdfs visited node =
        if not (List.mem node visited) then
          let s = successors node graph in
          List.fold_left rdfs (node::visited) s
        else visited
      in
      rdfs [] start
    in
    List.length (dfs roads (fst (List.hd roads))) - 1

let longest_road st =
  match
    List.fold_left (
      fun acc c ->
        let l = longest_road_length st c in
        if l > snd acc && l >= 5
        then (Some c, l) else acc
    ) (None, 0) [Red; Yellow; Blue; Green] |> fst
  with
  | None -> st
  | Some color ->
    let player = { (get_player color st) with longest_road = true } in
    let players = List.map (
        fun x -> if x.color = player.color
          then player else { x with longest_road = false }
      ) st.players in
    { st with players }

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
  in
  match possible_player with
  | None -> st
  | Some now_player ->
    let updated_player={now_player with largest_army=true} in
    let players = List.map (
        fun x ->
          if x.color = updated_player.color then updated_player
          else { x with largest_army = false }
      ) st.players in
    { st with players }

(*****************************************************************************
 *                                    DO                                     *
 *****************************************************************************)

let end_turn forward s =
  let rec index acc = function
    | [] -> raise Not_found
    | h :: t -> if h.color = s.turn then acc else index (1 + acc) t
  in
  let inc = if forward then 1 else 3 in
  let turn = (List.nth s.players ((index 0 s.players + inc) mod 4)).color in
  { s with turn }

let do_move cmd color_opt st =
  try
    match cmd with
    | InitSettlement i -> init_build_settlement i st.turn st
    | InitRoad e -> init_build_road e st.turn st
    | BuildSettlement i -> build_settlement i st
    | BuildCity i -> build_city i st
    | BuildRoad e -> build_road e st
    | BuyCard -> buy_card st
    | PlayKnight i -> play_knight i st
    | PlayRoadBuilding (e1, e2) -> play_road_build e1 e2 st
    | PlayYearOfPlenty (r1, r2) -> play_year_of_plenty r1 r2 st
    | PlayMonopoly r -> play_monopoly r st
    | Robber i -> play_robber i st
    | DomesticTrade (l1, l2) ->
      begin
        match color_opt with
        | None -> invalid_arg "Requires a color."
        | Some color -> domestic l1 l2 color st
      end
    | MaritimeTrade (p0, p1) ->
      begin
        match color_opt with
        | None -> invalid_arg "Requires a color."
        | Some color -> maritime [p0] [p1] st
      end
    | Discard lst ->
      begin
        match color_opt with
        | None -> invalid_arg "Requires a color."
        | Some color -> discard_resource color lst st
      end
    | EndTurn -> end_turn true st
    | _ -> st
  with
  | Invalid_argument msg | Failure msg -> print_endline msg; st (* TODO: Remove print statement. *)
  | _ -> st

(*****************************************************************************
 *                                   TEST                                    *
 *****************************************************************************)

let canvas_to_test =
  let center = 500., 375. in
  let length = 50. in
  let apothem =  length *. sqrt 3. /. 2. in
  {
    tiles = [
      { indices = [3; 4; 15; 14; 13; 2];
        dice = Some 3;
        resource = Some Ore;
        center = fst center -. 2. *. apothem, snd center +. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [5; 6; 17; 16; 15; 4];
        dice = Some 8;
        resource = Some Ore;
        center = fst center, snd center +. 3. *. length ;
        edge = length;
        buildings = [];
        roads = [((5,6),Green)] };
      { indices = [7; 8; 19; 18; 17; 6];
        dice = Some 10;
        resource = Some Ore;
        center = fst center +. 2. *. apothem, snd center +. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [13; 14; 25; 24; 23; 12];
        dice = Some 6;
        resource = Some Grain;
        center = fst center -. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [15; 16; 27; 26; 25; 14];
        dice = Some 4;
        resource = Some Grain;
        center = fst center -. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [17; 18; 29; 28; 27; 16];
        dice = Some 5;
        resource = Some Wool;
        center = fst center +. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [19; 20; 31; 30; 29; 18];
        dice = Some 9;
        resource = Some Brick;
        center = fst center +. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [23; 24; 35; 34; 33; 22];
        dice = None;
        resource = None;
        center = fst center -. 4. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [25; 26; 37; 36; 35; 24];
        dice = Some 9;
        resource = Some Lumber;
        center = fst center -. 2. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [27; 28; 39; 38; 37; 26];
        dice = Some 11;
        resource = Some Brick;
        center = fst center, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [29; 30; 41; 40; 39; 28];
        dice = Some 6;
        resource = Some Lumber;
        center = fst center +. 2. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = []};
      { indices = [31; 32; 43; 42; 41; 30];
        dice = Some 12;
        resource = Some Wool;
        center = fst center +. 4. *. apothem, snd center;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [35; 36; 47; 46; 45; 34];
        dice = Some 2;
        resource = Some Lumber;
        center = fst center -. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [37; 38; 49; 48; 47; 36];
        dice = Some 10;
        resource = Some Brick;
        center = fst center -. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [39; 40; 51; 50; 49; 38];
        dice = Some 3;
        resource = Some Grain;
        center = fst center +. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [41; 42; 53; 52; 51; 40];
        dice = Some 11;
        resource = Some Wool;
        center = fst center +. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [47; 48; 59; 58; 57; 46];
        dice = Some 5;
        resource = Some Lumber;
        center = fst center -. 2. *. apothem, snd center -. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [49; 50; 61; 60; 59; 48];
        dice = Some 8;
        resource = Some Grain;
        center = fst center, snd center -. 3. *. length ;
        edge = length;
        buildings = [];
        roads = [] };
      { indices = [51; 52; 63; 62; 61; 50];
        dice = Some 4;
        resource = Some Wool;
        center = fst center +. 2. *. apothem, snd center -. 3. *. length;
        edge = length;
        buildings = [];
        roads = [] };
    ];
    ports = [
      { neighbors = 2, 3;
        demand = None;
        rate = 3 };
      { neighbors = 5, 6;
        demand = Some Brick;
        rate = 2 };
      { neighbors = 12, 23;
        demand = None;
        rate = 3 };
      { neighbors = 19, 20;
        demand = Some Lumber;
        rate = 2 };
      { neighbors = 32, 43;
        demand = Some Grain;
        rate = 2 };
      { neighbors = 34, 45;
        demand = Some Ore;
        rate = 2 };
      { neighbors = 52, 53;
        demand = None;
        rate = 3 };
      { neighbors = 57, 58;
        demand = Some Wool;
        rate = 2 };
      { neighbors = 60, 61;
        demand = None;
        rate = 3 }
    ]
  }

let state_to_test =
  let players = [ {(Player.init_player Red) with wool = (Player.init_player Red).wool+5;
                                    lumber = (Player.init_player Red).lumber+5;
                                    grain = (Player.init_player Red).grain+5;
                                    brick = (Player.init_player Red).brick+5;
                                    ore =(Player.init_player Red).ore+5;};
                  {(Player.init_player Yellow) with wool = 5; lumber = 5;
                                      grain = 5; brick = 5; ore =5;};
                  {(Player.init_player Blue) with wool = 5; lumber = 5;
                                       grain = 5; brick = 5; ore =5;};
                  {(Player.init_player Green) with wool = 5; lumber = 5;
                                       grain = 5; brick = 5; ore =5;}; ] in
  { robber = 10;
    deck = [ YearOfPlenty; VictoryPoint; Knight; RoadBuilding;YearOfPlenty;Knight;
             Knight; RoadBuilding; Knight; Knight; VictoryPoint; Knight;
             Knight; Monopoly; Knight; YearOfPlenty; Knight; Knight;
             VictoryPoint; Knight; Knight; Knight; Monopoly; VictoryPoint;
             Knight; VictoryPoint ];
    players;
    turn = (List.hd players).color;
    canvas = canvas_to_test }

let score color st =
    let build_score =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.filter (fun (ind, (c, rate)) -> c=color)
    |> List.map (fun (ind, (c, rate)) -> rate)
    |> List.fold_left (fun acc x -> acc + x) 0
  in
  let player = get_player color st in
  let victory_card_score = player.victory_point in
  let longest_road_score = if player.longest_road then 2 else 0 in
  let largest_army_score = if player.largest_army then 2 else 0 in
  build_score + victory_card_score + longest_road_score + largest_army_score

let check_win color st = score color st >= 10

let settlements color st =
  List.fold_left (
    fun acc t ->
      List.fold_left (
        fun acc (index, (c, multiplier)) ->
          if c = color && multiplier = 1 then index :: acc else acc
      ) [] t.buildings
  ) [] st.canvas.tiles
  |> List.sort_uniq compare

let cities color st =
  List.fold_left (
    fun acc t ->
      List.fold_left (
        fun acc (index, (c, multiplier)) ->
          if c = color && multiplier = 2 then index :: acc else acc
      ) [] t.buildings
  ) [] st.canvas.tiles
  |> List.sort_uniq compare

let roads color st =
  let cmp (s1, e1) (s2, e2) =
    if s1 < s2 then -1
    else if s1 > s2 then 1
    else if e1 < e2 then -1
    else if e1 > e2 then 1
    else 0
  in
  List.fold_left (
    fun acc t ->
      List.fold_left (
        fun acc ((i0, i1), c) ->
          if c <> color then acc
          else if i0 < i1 then (i0, i1) :: acc else (i1, i0) :: acc
      ) [] t.roads
  ) [] st.canvas.tiles
  |> List.sort_uniq cmp

let ports color st =
  let smts = settlements color st in
  let cmp (s1, e1) (s2, e2) =
    if s1 < s2 then -1
    else if s1 > s2 then 1
    else if e1 < e2 then -1
    else if e1 > e2 then 1
    else 0
  in
  List.fold_left (
    fun acc x ->
      let i0 = fst x.neighbors in
      let i1 = snd x.neighbors in
      if List.mem i0 smts || List.mem i1 smts
      then x.neighbors :: acc else acc
  ) [] st.canvas.ports
  |> List.sort_uniq cmp

let cards color st =
  let player = get_player color st in
  List.filter (
    fun (r, i) -> i > 0
  ) [ Knight, player.knight; RoadBuilding, player.road_building;
      YearOfPlenty, player.year_of_plenty; Monopoly, player.monopoly;
      VictoryPoint, player.victory_point ]

let resources color st =
  let player = get_player color st in
  List.filter (
    fun (r, i) -> i > 0
  ) [ Lumber, player.lumber; Wool, player.wool; Grain, player.grain;
      Brick, player.brick ]

let robber st = st.robber

let turn st = st.turn
