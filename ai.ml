open Elements

open State

open Tile

(* [get_probability_dice num] gets the relative probability for dice number
 * [num] to appear *)
let get_probability_dice num =
  match num with
  | 2  -> 3
  | 3  -> 6
  | 4  -> 8
  | 5  -> 11
  | 6  -> 14
  | 7  -> 17
  | 8  -> 14
  | 9  -> 11
  | 10 -> 8
  | 11 -> 6
  | 12 -> 3
  | _  -> 0

(* [get_probability_card st res] returns the number of card [res] remaining
 * in game's holding decks at state [st], so to see their relative probability. *)
let get_probability_card st res =
  let remaining_cards = st.deck in
  let count res =
    List.fold_left (fun acc x -> if x = res then acc+1 else acc)
      0 remaining_cards in
  match res with
  | Knight -> count Knight
  | RoadBuilding -> count RoadBuilding
  | YearOfPlenty -> count YearOfPlenty
  | Monopoly -> count Monopoly
  | VictoryPoint -> count VictoryPoint

(* [get_possible_house_ind st col f] returns a list of
 * possible indexes on which we can build a settlement or city based on
 * different checking functions [f]. *)
let get_possible_house_ind st col f =
  let rec from i j acc = if i > j then acc else from i (j-1) (j::acc) in
  let lst = (from 2 8 []) @ (from 12 20 []) @ (from 22 43 []) @
            (from 45 52 []) @ (from 57 63 []) in
  List.filter (fun x -> f col x st) lst

let can_build_settlement_ai col x st =
  try can_build_settlement col x st with
  | _ -> false

let can_build_city_ai col x st =
  try can_build_city col x st with
  | _ -> false

(* [get_accesible res col st] returns a list of resources obtainable for player
 * identified by color [col] at state [st] *)
let get_accessible_resources col st =
  st.canvas.tiles
    |> List.map (fun x -> (x.buildings, x.resource))
    |> List.map (fun (l, r) -> List.map (fun (_,(c, _)) -> (c, r)) l)
    |> List.flatten
    |> List.sort_uniq compare
    |> List.map (fun (c, r) -> r)

let obtainable_resources ind st =
  st.canvas.tiles
  |> List.filter (fun x -> List.mem ind x.indices)
  |> List.map (fun x -> x.resource)

let initial_resource_priority = function
  | Lumber -> 3
  | Wool -> 2
  | Brick -> 3
  | Ore -> 2
  | Grain -> 1
  | Null -> 0

let obtain_bordering_dices ind st =
  st.canvas.tiles
  |> List.filter (fun x -> List.mem ind x.indices)
  |> List.map (fun x -> x.dice)

let calc_value_settlement ind st =
  let resources = obtainable_resources ind st in
  let dices = obtain_bordering_dices ind st in
  let res_pts =
    resources
    |> List.map (fun x -> 5 * (initial_resource_priority x))
    |> List.fold_left (fun acc x -> acc + x) 0
  in
  let dice_pts =
    dices
    |> List.map (fun x -> get_probability_dice x)
    |> List.fold_left (fun acc x -> acc + x) 0
  in
  res_pts + dice_pts

(* for buildings, consider:
 * 1. how many VP the building is worth
 * 2. given current resources, figure out what to build or to wait
 * 3. by building it, what other building options for the future becomes avaiable *)

(* 1. dice num with higher prob  2. covers more resources with good priority *)
let init_choose_first_settlement_build st col =
  let possible_ind = get_possible_house_ind st col can_build_settlement_ai in
  let values = List.map (fun x -> (calc_value_settlement x st, x)) possible_ind in
  let info =
    List.fold_left (fun (accx, accy) (x, y) ->
        if x > accx then (x, y) else (accx, accy)) (-1, -1) values in
  (snd info)

let init_possible_roads st col ind =
  let border_roads = fetch_neighbors ind in
  List.filter (fun x -> check_initialize_build_road (ind, x) st col) border_roads

let init_choose_road_build st col ind =
  let possible_roads = init_possible_roads st col ind in
  let i1 = Random.int (List.length possible_roads) in
  (ind, List.nth possible_roads i1)

let init_choose_second_settlement_build st col = failwith "TODO"

let make_build_plan = failwith "TODO"

let index_obtainable_in_one_road = failwith "TODO"

let index_obtainable_in_two_roads = failwith "TODO"

let choose_road = failwith "TODO"

let choose_settlement = failwith "TODO"

let choose_city = failwith "TODO"

let want_build_settlement = failwith "TODO"

let want_build_road = failwith "TODO"

let want_build_city = failwith "TODO"

let want_init_trade = failwith "TODO"

let init_trade = failwith "TODO"

let want_accept_trade_player = failwith "TODO"

let want_trade_bank = failwith "TODO"

let want_trade_ports = failwith "TODO"

let want_buy_card = failwith "TODO"

let want_play_monopoly = failwith "TODO"

(* if has a favorable rate then do that one otherwise choose the most many resource *)
let choose_monopoly = failwith "TODO"

let want_play_year_of_plenty = failwith "TODO"

let choose_two_plenty_resource = failwith "TODO"

let want_play_road_building = failwith "TODO"

let want_play_knight = failwith "TODO"

let choose_robber_spot = failwith "TODO"

(* if with a plan then keep resource for it else random *)
let choose_discard_resource = failwith "TODO"

let want_end_turn = failwith "TODO"

let robot_times_out = failwith "TODO"

let do_ai = failwith "TODO"
