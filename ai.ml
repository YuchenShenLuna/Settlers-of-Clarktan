open Elements
open State
open Player
open Tile

(*****************************************************************************
 *                                  HELPERS                                  *
 *****************************************************************************)

(* plan is the type of next building plan for ai *)
type plan =
 | Build_Settlement of int
 | Build_City of int
 | Build_Road of edge * plan
 | Neither of plan

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

(* [obtain_score color st] returns the number of victory points the player
 * identified by color [color] has in state [st] *)
let obtain_score color st =
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  player.score

(* [resource_priority_diff_stage color st res] returns the priority of resource
 * [res] for player with color [color] under state [st] *)
let resource_priority_diff_stage color st res =
  let score = obtain_score color st in
  if score < 5 then
    match res with
    | Lumber -> 3
    | Wool   -> 2
    | Brick  -> 3
    | Ore    -> 2
    | Grain  -> 1
    | Null   -> 0
  else if score < 9 then
    match res with
    | Lumber -> 2
    | Wool   -> 1
    | Brick  -> 2
    | Ore    -> 3
    | Grain  -> 3
    | Null   -> 0
  else
    match res with
    | Lumber -> 1
    | Wool   -> 2
    | Brick  -> 1
    | Ore    -> 3
    | Grain  -> 3
    | Null   -> 0

(* [list_of_resources color st] returns the list of resources under state [st]
 * for player identified by color [color] *)
let list_of_resources color st =
  let rec f r n acc = if n <= 0 then acc else f r (n - 1) (r :: acc) in
  let cons r = f r (num_resource color r st) in
  [] |> cons Lumber |> cons Wool |> cons Grain |> cons Brick |> cons Ore

(* [take n acc lst] returns the first n elements of list [lst] *)
let rec take n acc = function
  | [] -> acc
  | h :: t -> if n <= 0 then acc else take (n - 1) (h :: acc) t

(*****************************************************************************
 *                                STRATEGY                                   *
 *****************************************************************************)

(* TODO: Potentially lay out different strategies. *)

(*****************************************************************************
 *                              INITIAL PHASE                                *
 *****************************************************************************)

(* [get_possible_house_ind st col f] returns a list of
 * possible indexes on which we can build a settlement or city based on
 * different checking functions [f]. *)
let get_possible_house_ind st col f =
  let rec from i j acc = if i > j then acc else from i (j-1) (j::acc) in
  let lst = (from 2 8 []) @ (from 12 20 []) @ (from 22 43 []) @
            (from 45 52 []) @ (from 57 63 []) in
  List.filter (fun x -> f col x st) lst

(* [init_can_build_settlement_ai col x st] returns whether an ai identified by
 * color [col] can build a settlement at index [x] at initial state [st] *)
let init_can_build_settlement_ai col x st =
  try check_initialize_build_settlement x st with
  | _ -> false

(* [can_build_settlement_ai col x st] returns whether an ai identified by
 * color [col] can build a settlement at index [x] under state [st] after
 * the initial building phase ends *)
let can_build_settlement_ai col x st =
  try can_build_settlement col x st with
  | _ -> false

(* [can_build_settlement_ai col x st] returns whether an ai identified by
 * color [col] can build a city at index [x] under state [st] after
 * the initial building phase ends *)
let can_build_city_ai col x st =
  try can_build_city col x st with
  | _ -> false

(* [get_possible_settlement_ind st col f] returns a list of indices that
 * the ai identified by color [col] can build at current state [st] under
 * the rule of the checking function [f] *)
let get_possible_settlement_ind st col f =
  get_possible_house_ind st col can_build_settlement_ai

(* [get_possible_city_ind st col f] returns a list of indices that
 * the ai identified by color [col] can build at current state [st] under
 * the rule of the checking function [f] *)
let get_possible_city_ind st col f =
  get_possible_house_ind st col can_build_city_ai

(* [get_accesible res col st] returns a list of resources obtainable for player
 * identified by color [col] at state [st] *)
let get_accessible_resources col st =
  st.canvas.tiles
  |> List.map (fun x -> (x.buildings, x.resource))
  |> List.map (fun (l, r) -> List.map (fun (_,(c, _)) -> (c, r)) l)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.map (fun (c, r) -> r)

(* [get_accessible_resources_by_inds col lst st] returns a list of resources
 * obtainable for player identified by color [col] at state [st] given by
 * the indices of buildings specified by [lst] *)
let get_accessible_resources_by_inds col lst st =
  let tiles = st.canvas.tiles in
  let get_tile ind = List.filter (fun x -> List.mem ind x.indices) tiles in
  lst
  |> List.map (fun x -> get_tile x)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.map (fun x -> x.resource)

(* [get_unaccessible_resources col st] returns a list of resources not
 * obtainable for player identified by color [col] at state [st] *)
let get_unaccessible_resources col st =
  let res_lst = [Lumber; Ore; Grain; Brick; Wool] in
  let accessibles = get_accessible_resources col st in
  List.filter (fun x -> List.mem x accessibles = false) res_lst

(* [obtainable_resources ind st] returns a list of obtainable resources
 * obtained by adding the index [ind] to the players' building lists under
 * current state [st] *)
let obtainable_resources ind st =
  st.canvas.tiles
  |> List.filter (fun x -> List.mem ind x.indices)
  |> List.map (fun x -> x.resource)

(* [initial_resource_priority res] is the priority for resource [res] at
 * initial building phase of the game *)
let initial_resource_priority = function
  | Lumber -> 3
  | Wool -> 2
  | Brick -> 3
  | Ore -> 2
  | Grain -> 1
  | Null -> 0

(* [obtain_bordering_dices ind st] returns a list of dice numbers bordering the
 * house indexed by [ind] under state [st] *)
let obtain_bordering_dices ind st =
  st.canvas.tiles
  |> List.filter (fun x -> List.mem ind x.indices)
  |> List.map (fun x -> x.dice)

(* [init_calc_value_settlement ind st f] calculates the initial value for the
 * settlement at index [ind] under the current state [st] decided by checking
 * function [f] *)
let init_calc_value_settlement ind st f =
  let resources = obtainable_resources ind st in
  let dices = obtain_bordering_dices ind st in
  let res_pts =
    resources
    |> List.map (fun x -> 5 * (f x))
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

let init_choose_first_settlement_build st col =
  let possible_ind = get_possible_house_ind st col init_can_build_settlement_ai in
  let values =
    List.map (fun x ->
        (init_calc_value_settlement x st initial_resource_priority, x))
      possible_ind
  in
  let info =
    List.fold_left (fun (accx, accy) (x, y) ->
        if x > accx then (x, y) else (accx, accy)) (-1, -1) values in
  (snd info)

(* [init_possible_roads st col ind] returns a list of roads possible to be
 * built for player identified by color [col] under state [st] due to building
 * settlement at index [ind] *)
let init_possible_roads st col ind =
  let border_roads = fetch_neighbors ind in
  List.filter (fun x -> check_initialize_build_road (ind, x) st col) border_roads

let init_choose_road_build st col ind =
  let possible_roads = init_possible_roads st col ind in
  let i1 = Random.int (List.length possible_roads) in
  (ind, List.nth possible_roads i1)

(* [is_sublist lst1 lst2] returns whether lst1 is a sublist of lst2 regardless
 * of orderings *)
let is_sublist lst1 lst2 =
  List.fold_left (fun acc x -> acc && (List.mem x lst2)) true lst1

let init_choose_second_settlement_build st col =
  let possible_ind = get_possible_house_ind st col init_can_build_settlement_ai in
  let needed_res = get_unaccessible_resources col st in
  let info =
    possible_ind
    |> List.filter (fun x -> is_sublist needed_res (obtainable_resources x st))
  in
  if List.length info <> 0 then
    let values =
      List.map (fun x ->
          (init_calc_value_settlement x st initial_resource_priority, x)) info
    in
    let lst =
    List.fold_left (fun (accx, accy) (x, y) ->
          if x > accx then (x, y) else (accx, accy)) (-1, -1) values in
    (snd lst)
  else
    let needed_res' = [List.hd needed_res] in
    let info' =
      possible_ind
      |> List.filter (fun x -> is_sublist needed_res' (obtainable_resources x st))
    in
    if List.length info' <> 0 then
      let values' =
        List.map (fun x ->
            (init_calc_value_settlement x st initial_resource_priority, x)) info'
      in
      let lst' =
        List.fold_left (fun (accx, accy) (x, y) ->
            if x > accx then (x, y) else (accx, accy)) (-1, -1) values' in
      (snd lst')
    else
      init_choose_first_settlement_build st col

(*****************************************************************************
 *                                   BUILD                                   *
 *****************************************************************************)

(* [calc_value_house ind st color] determines the value of the house (either a
 * settlement or a city) built at index [ind] by player identified by color
 * [col] under state [st] *)
let calc_value_house ind st color =
  let resources = obtainable_resources ind st in
  let dices = obtain_bordering_dices ind st in
  let res_pts =
    resources
    |> List.map (fun x -> 8 * (resource_priority_diff_stage color st x))
    |> List.fold_left (fun acc x -> acc + x) 0
  in
  let dice_pts =
    dices
    |> List.map (fun x -> get_probability_dice x)
    |> List.fold_left (fun acc x -> acc + x) 0
  in
  res_pts + dice_pts

(* [has_settlement ind st] returns whether there is a settlement build at
 * index [ind] at state [st] *)
let has_settlement ind st =
  let info =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
    |> List.sort_uniq compare
  in
  List.assoc_opt ind info <> None

(* [index_obtainable_in_one_road st color] returns a list of indices upon
 * which a settlement can potentially be built after the player identified
 * by color [col] builds one more road at state [st] *)
let index_obtainable_in_one_road st color =
  st.canvas.tiles
  |> List.map (fun x -> x.roads)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.filter (fun (edge, col) -> col = color)
  |> List.map (fun ((a1, a2), b) -> [a1; a2])
  |> List.flatten
  |> List.filter (fun x -> has_settlement x st = false)
  |> List.map (fun x -> fetch_neighbors x)
  |> List.flatten
  |> List.filter (fun x -> check_build_settlement x st color)

(* [roads_in_one st color] returns a list of roads so that they reach the
 * settlement index upon which a settlement can potentially be built
 * after the player identified by color [col] builds one more road at state [st]*)
let roads_in_one st color =
  st.canvas.tiles
  |> List.map (fun x -> x.roads)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.filter (fun (edge, col) -> col = color)
  |> List.map (fun ((a1, a2), b) -> [a1; a2])
  |> List.flatten
  |> List.filter (fun x -> has_settlement x st = false)
  |> List.map (fun x -> (x, fetch_neighbors x))
  |> List.map (fun (a, b) -> List.map (fun y -> (a, y)) b)
  |> List.flatten
  |> List.filter (fun (a, b) -> check_build_settlement b st color)

(* [fetch_road_in_one ind st color] fetches the index of the road to be built
 * by player identified by color [color] under state [st] so as to reach the
 * settlement indexed by [ind] *)
let fetch_road_in_one ind st color =
  let help ind st color =
    let roads = roads_in_one st color in
    if List.assoc_opt ind roads <> None then
      (List.assoc_opt ind roads, Some ind)
    else
      let roads_reversed = List.map (fun (a, b) -> (b, a)) roads in
      (List.assoc_opt ind roads_reversed, Some ind)
  in
  let result = help ind st color in
  match result with
  | Some a, Some b -> (a, b)
  | _, _ -> (-1, -1)

(* [index_obtainable_in_two_roads st color] returns a list of indices upon
 * which a settlement can potentially be built after the player identified
 * by color [col] builds two more roads at state [st] *)
let index_obtainable_in_two_roads st color =
  st.canvas.tiles
  |> List.map (fun x -> x.roads)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.filter (fun (edge, col) -> col = color)
  |> List.map (fun ((a1, a2), b) -> [a1; a2])
  |> List.flatten
  |> List.map (fun x -> fetch_neighbors x)
  |> List.flatten
  |> List.map (fun x -> fetch_neighbors x)
  |> List.flatten
  |> List.filter (fun x -> check_build_settlement x st color)

(* [roads_in_two st color] returns a list of roads so that they reach the
 * settlement index upon which a settlement can potentially be built
 * after the player identified by color [col] builds two more roads
 * at state [st]*)
let roads_in_two st color =
  st.canvas.tiles
  |> List.map (fun x -> x.roads)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.filter (fun (edge, col) -> col = color)
  |> List.map (fun ((a1, a2), b) -> [a1; a2])
  |> List.flatten
  |> List.map (fun x -> (x, fetch_neighbors x))
  |> List.map (fun (a, b) -> List.map (fun y -> (a, y)) b)
  |> List.flatten
  |> List.map (fun (a, b) -> ((a, b), fetch_neighbors b))
  |> List.map (fun ((a, b), lst) -> List.map (fun y -> ((a, b), (b, y))) lst)
  |> List.flatten

(* [fetch_roads_in_two ind st color] fetches a list of the index of the road
 * to be built by player identified by color [color] under state [st] so
 * as to reach the settlement indexed by [ind] *)
let fetch_roads_in_two ind st color =
  let info = roads_in_two st color in
  if info= [] then []
  else
    List.filter (fun ((a, b), (c, y)) -> y = ind) info

(* [choose_settlement st color] returns the index of the settlement the ai
 * identified by color [color] wants to build next at state [st] *)
let choose_settlement st color =
  let possibles = get_possible_settlement_ind st color can_build_settlement_ai in
  List.fold_left
    (fun acc x -> if calc_value_house x > calc_value_house acc
      then x else acc) (List.hd possibles) possibles

(* [roads] represent all potential edges upon which roads can be build
 * throughout the entire game *)
let roads =
  [(3, 4); (4, 15); (15, 14); (14, 13); (13, 2); (2, 3); (5, 6); (6, 17);
   (17, 16); (16, 15); (15, 4); (4, 5); (7, 8); (8, 19); (19, 18); (18, 17);
   (17, 6); (6, 7); (13, 14); (14, 25); (25, 24); (24, 23); (23, 12); (12, 13);
   (15, 16); (16, 27); (27, 26); (26, 25); (25, 14); (14, 15); (17, 18);
   (18, 29); (29, 28); (28, 27); (27, 16); (16, 17); (19, 20); (20, 31);
   (31, 30); (30, 29); (29, 18); (18, 19); (23, 24); (24, 35); (35, 34);
   (34, 33); (33, 22); (22, 23); (25, 26); (26, 37); (37, 36); (36, 35);
   (35, 24); (24, 25); (27, 28); (28, 39); (39, 38); (38, 37); (37, 26);
   (26, 27); (29, 30); (30, 41); (41, 40); (40, 39); (39, 28); (28, 29);
   (31, 32); (32, 43); (43, 42); (42, 41); (41, 30); (30, 31); (35, 36);
   (36, 47); (47, 46); (46, 45); (45, 34); (34, 35); (37, 38); (38, 49);
   (49, 48); (48, 47); (47, 36); (36, 37); (39, 40); (40, 51); (51, 50);
   (50, 49); (49, 38); (38, 39); (41, 42); (42, 53); (53, 52); (52, 51);
   (51, 40); (47, 48); (48, 51); (51, 58); (58, 57); (57, 46); (46, 47);
   (49, 50); (50, 61); (61, 60); (60, 59); (59, 48); (48, 49); (51, 52);
   (52, 63); (63, 62); (62, 61); (61, 50); (50, 51)]

(* [get_the_road st col] returns the road for player identified by color
 * [col] under state [st] in goal of reaching the longest road token,
 * only used after all other building road plans have failed. *)
let get_the_road st col =
  let info = List.filter (fun x -> check_build_road x st col) roads in
  if info = [] then
    None
  else
    let update_tiles rd =
      let tiles = st.canvas.tiles in
      tiles
      |> List.map (fun x ->
          if List.mem (fst rd) x.indices && List.mem (snd rd) x.indices then
            {x with roads = (rd, col)::x.roads}
          else x)
    in
    let get_state rd = {st with canvas = {tiles = update_tiles rd;
                                          ports = st.canvas.ports}} in
    Some (List.fold_left
            (fun acc x -> if longest_road_length (get_state x) col >
                             longest_road_length (get_state acc) col then x
              else acc) (0, 0) info)

(* [choose_road ind color st] returns the next road the ai identified by color
 * [color] should build under state [st] *)
let choose_road ind color st =
  let ind_one = index_obtainable_in_one_road st color in
  let ind_two = index_obtainable_in_two_roads st color in
  if List.mem ind ind_one then
    fetch_road_in_one ind st color
  else if List.mem ind ind_two then
    (fetch_roads_in_two ind st color |> List.hd |> fst)
  else
    match get_the_road st color with
    | None -> failwith "no road building possible"
    | Some r -> r

(* [choose_city st color] returns the index of the city the ai identified
 * by color [color] should seek to upgrade to at current state [st] *)
let choose_city st color =
  let possibles = get_possible_city_ind st color can_build_city_ai in
  List.fold_left
    (fun acc x -> if calc_value_house x > calc_value_house acc
      then x else acc) (List.hd possibles) possibles

(* [enough_res_for_settlement st color] returns whether the player has
 * obtained enough resources for building a settlement *)
let enough_res_for_settlement st color =
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  player.ore > 0 && player.wool > 0 && player.lumber > 0 && player.brick > 0

(* [enough_res_for_city st color] returns whether the player has
 * obtained enough resources for building a city *)
let enough_res_for_city st color =
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  player.ore > 2 && player.grain > 3

(* [enough_res_for_road st color] returns whether the player has
 * obtained enough resources for building a road *)
let enough_res_for_road st color =
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  player.lumber > 0 && player.brick > 0

(* [possible_city st color] returns a list of indices of cities the ai
 * identified by color [color] can upgrade to under state [st] *)
let possible_city st color =
  st.canvas.tiles
  |> List.map (fun x -> x.buildings)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.filter (fun (a, (col, t)) -> col = color && t = 1)
  |> List.map (fun (a, (col, t)) -> a)
  |> List.sort_uniq compare

(* [make_build_plan st color] returns a building plan of type [plan]
 * for ai identified by color [color] under state [st] *)
let make_build_plan st color =
  let can_settlement = enough_res_for_settlement st color in
  let can_city = enough_res_for_city st color in
  let can_road = enough_res_for_road st color in
  if can_settlement && can_city then
    let settlement = choose_settlement st color in
    let city = choose_city st color in
    if calc_value_house settlement > calc_value_house city then
      Build_Settlement settlement
    else
      Build_City city
  else if can_settlement then
    let settlement = choose_settlement st color in
    Build_Settlement settlement
  else if can_city then
    let city = choose_city st color in
    Build_City city
  else if can_road then
    let settlement_one =
      index_obtainable_in_one_road st color
      |> List.map (fun x -> (calc_value_house x st color, x))
      |> List.fold_left (fun (acc1, acc2) (a, b) ->
          if a > acc1 then (a, b) else (acc1, acc2)) (0, 0)
      |> snd
    in
    let settlement_two =
    index_obtainable_in_two_roads st color
    |> List.map (fun x -> (calc_value_house x st color, x))
    |> List.fold_left (fun (acc1, acc2) (a, b) -> if a > acc1 then (a, b)
                        else (acc1, acc2)) (0, 0)
    |> snd
    in
    let city =
      possible_city st color
      |> List.map (fun x -> (calc_value_house x st color, x))
      |> List.fold_left (fun (acc1, acc2) (a, b) -> if a > acc1 then (a, b)
                          else (acc1, acc2)) (0, 0)
      |> snd
    in
    let val_set1 = calc_value_house settlement_one in
    let val_set2 = calc_value_house settlement_two in
    let val_c = calc_value_house city in
    if val_set1 >= val_set2 && val_set1 >= val_c then
      Build_Road ((choose_road settlement_one color st),
                  Build_Settlement settlement_one)
    else if val_set2 >= val_set1 && val_set2 >=val_c then
      Build_Road ((choose_road settlement_two color st),
                  Build_Settlement settlement_two)
    else
      Neither (Build_City city)
  else
    Neither (Build_Road ((0, 0), Build_Settlement 0))

(* [want_build_settlement color st] returns whether the ai wants to build a
 * settlement at state [st]. The ai is identified by color [color]. *)
let want_build_settlement color st =
  match make_build_plan st color with
  | Build_Settlement _ -> true
  | _ -> false

(* [want_build_road color st] returns whether the ai wants to build a
 * road at state [st]. The ai is identified by color [color]. *)
let want_build_road color st =
  match make_build_plan st color with
  | Build_Road _ -> true
  | _ -> false

(* [want_build_city color st] returns whether the ai wants to build a
 * city at state [st]. The ai is identified by color [color]. *)
let want_build_city color st =
  match make_build_plan st color with
  | Build_City _ -> true
  | _ -> false

(* [want_buy_card color st] returns whether the ai wants to buy a development
 * card under state [st]. The ai is identified by color [color]. *)
let want_buy_card color st =
  num_resource color Wool st > 0
  && num_resource color Grain st > 0
  && num_resource color Ore st > 0
  && not (want_build_settlement color st)
  && not (want_build_road color st)
  && not (want_build_city color st)
  && (num_all_resources color st > 7
      || failwith "TODO")

(* [get_next_resources p] returns a list of resources you would need most at
 * current state for the current player, the [p] should be the result for
 * (make_build_plan st color) *)
let rec get_next_resources p =
  match p with
  | Neither (Build_Road _) -> [Brick; Lumber]
  | Neither (Build_City _) -> [Ore; Grain]
  | Build_Road (_, p') -> get_next_resources p'
  | Build_Settlement _ -> [Brick; Lumber]
  | _ -> [Brick; Lumber]

(*****************************************************************************
 *                                   TRADE                                   *
 *****************************************************************************)


(*general function to judge whether the ai player should trade, mainly deal with
  some extreme cases*)
let want_to_trade st ai rs_list=
  (* if some kind of resource of ai that need to use to trade is less than 2, then do not trade*)
  let check_resource_use_to_trade rs_list= List.map
      (fun (r,n) -> match r with
         | Ore -> ai.ore >=2
         | Lumber -> ai.lumber>=2
         | Wool -> ai.wool>=2
         | Grain -> ai.grain>=2
         | Brick -> ai.brick >=2
         | Null ->  0 = 0) rs_list in
 not (List.mem false  (check_resource_use_to_trade rs_list))


(*helper function calculate the potential score given the current state
  and current ai player (and its resources)*)
let potential_score_not_trade st ai =
  let res = get_next_resources (make_build_plan st ai.color) in
  let score r = if List.mem r res then 7 else resource_priority_diff_stage ai.color st r in
  (score Lumber) * ai.lumber +
  (score Wool) * ai.wool +
  (score Brick) * ai.brick +
  (score Grain) * ai.grain +
  (score Ore) * ai.ore

(*helper function calculates the potential score by replacing the resource in rs_list with
  corresponding resource in rs'_list*)
let potential_score_trade_with_other_player st ai rs_list rs'_list =failwith "unimplemented"

let want_accept_trade_player st ai rs_list other_pl rs'_list=
  (potential_score_not_trade st ai < potential_score_trade_with_other_player st ai rs_list rs'_list) &&
  not (List.mem Ore (List.map (fun (r,n) -> r) rs_list)) && other_pl.score <=6

let want_init_trade st ai rs_list other_pl rs'_list=
  (*other_pl.score <= 7 && rs <> Ore && num_resource other_pl.color rs' st > 0*)
  (potential_score_not_trade st ai < potential_score_trade_with_other_player st ai rs_list rs'_list) &&
  List.mem Ore (List.map (fun (r,n) -> r) rs_list) && other_pl.score <=6


let want_to_trade_with_all_other_players st pl pl_list rs rs'=
  let boolean_list =
    List.map (fun x-> x.color=pl.color || want_init_trade st pl rs x rs') pl_list in
  let number_of_true= List.fold_left (fun acc x-> if x then acc+1 else acc) 0 boolean_list in
  number_of_true = 4

let want_trade_bank st ai rs rs' =
  want_to_trade st ai rs
  && List.length (ports_of_player st ai.color) = 0
  && not (want_to_trade_with_all_other_players st ai st.players rs rs')

let want_trade_ports st ai rs rs'=
  List.length (ports_of_player_with_specific_resource st ai.color rs') > 0
  && not (want_to_trade_with_all_other_players st ai st.players rs rs')

(*****************************************************************************
 *                               ACHIEVEMENTS                                *
 *****************************************************************************)

let want_largest_army color st = failwith "TODO"

let want_longest_road color st = failwith "TODO"

(*****************************************************************************
 *                             DEVELOPMENT CARDS                             *
 *****************************************************************************)

let num_card color card s =
  let player = get_player color s in
  match card with
  | Knight -> player.knight
  | RoadBuilding -> player.road_building
  | YearOfPlenty -> player.year_of_plenty
  | Monopoly -> player.monopoly
  | VictoryPoint -> player.victory_point

let num_all_cards color s =
  num_card color Knight s
  + num_card color RoadBuilding s
  + num_card color YearOfPlenty s

let has_card color card s = num_card color card s > 0

let blocked color s = failwith "TODO"
let blocked_bad color s = failwith "TODO"
let bought_this_turn color s card = failwith "TODO"

let want_play_knight color s =
  num_all_resources color s <= 7
  && not (bought_this_turn color s Knight)
  && (blocked_bad color s
      || List.fold_left (
        fun acc p ->
          if p.color != color then not (blocked_bad p.color s) || acc else acc
      ) false s.players
      || want_largest_army color s
      || num_all_cards color s > 10)

let want_play_road_building color s =
  has_card color RoadBuilding s
  && failwith "TODO"

let want_play_year_of_plenty color s =
  has_card color YearOfPlenty s
  && failwith "TODO"

let want_play_monopoly color s =
  has_card color Monopoly s
  && failwith "TODO"

let choose_two_roads color s = failwith "TODO"

let choose_robber_spot color s =
  let robber_opt color s =
    let ok = List.fold_left (fun acc (_, (c, _)) -> acc && c <> color) true in
    let candidates = List.filter (fun t -> ok t.buildings) s.canvas.tiles in
    let num_buildings tile =
      tile.buildings
      |> List.filter (fun (_, (c, _)) -> c <> White)
      |> List.length
    in
    List.fold_left (
      fun acc t ->
        let len = num_buildings t in
        if snd acc < len then Some t, len else acc
    ) (None, 0) candidates |> fst
  in
  match robber_opt color s with
  | None -> List.hd s.canvas.tiles
  | Some tile -> tile

let choose_two_resources color s =
  let player = get_player color s in
  let hand = [ Lumber; Lumber; Wool; Wool; Grain;
               Grain; Brick; Brick; Ore; Ore ] in
  let value =
    if want_build_settlement color s then
      function
      | Lumber -> 100 - player.lumber
      | Wool -> 100 - player.wool
      | Grain -> 100 - player.grain
      | Brick -> 100 - player.brick
      | Ore -> 0
      | Null -> 0
    else
    if want_build_city color s then
      function
      | Lumber -> 0
      | Wool -> 0
      | Grain -> 200 - player.lumber
      | Brick -> 0
      | Ore -> 300 - player.ore
      | Null -> 0
    else if want_build_road color s then
      function
      | Lumber -> 100 - player.lumber
      | Wool -> 0
      | Grain -> 0
      | Brick -> 100 - player.brick
      | Ore -> 0
      | Null -> 0
    else if want_buy_card color s then
      function
      | Lumber -> 0
      | Wool -> 100 - player.wool
      | Grain -> 100 - player.grain
      | Brick -> 0
      | Ore -> 100 - player.ore
      | Null -> 0
    else function _ -> 0
  in
  let cmp a b = compare (value a) (value b) in
  match hand |> shuffle |> List.sort cmp |> List.rev with
  | h :: x :: _ -> h, x
  | _ -> failwith "Impossible"

let choose_monopoly color s = failwith "TODO"

(*****************************************************************************
 *                              MISCELLANEOUS                                *
 *****************************************************************************)

let choose_discard_resource color s =
  let hand = list_of_resources color s |> shuffle in
  let value =
    if want_build_settlement color s then
      function
      | Lumber -> 1
      | Wool -> 1
      | Grain -> 1
      | Brick -> 1
      | Ore -> 0
      | Null -> 0
    else
    if want_build_city color s then
      function
      | Lumber -> 0
      | Wool -> 0
      | Grain -> 2
      | Brick -> 0
      | Ore -> 3
      | Null -> 0
    else if want_build_road color s then
      function
      | Lumber -> 1
      | Wool -> 0
      | Grain -> 0
      | Brick -> 1
      | Ore -> 0
      | Null -> 0
    else if want_buy_card color s then
      function
      | Lumber -> 0
      | Wool -> 1
      | Grain -> 1
      | Brick -> 0
      | Ore -> 1
      | Null -> 0
    else function _ -> 0
  in
  let cmp a b = compare (value a) (value b) in
  hand |> List.sort cmp |> take (List.length hand / 2) []

let time_out = failwith "TODO"

(*****************************************************************************
 *                                    DO                                     *
 *****************************************************************************)

let do_ai = failwith "TODO"
