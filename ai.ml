open Elements

open State

open DevCard

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
 * different checking functions [f]. [f] is either can_build_settlements
 * or can_build_city *)
let get_possible_house_ind st col f =
  let rec from i j acc = if i > j then acc else from i (j-1) (j::acc) in
  let lst = (from 2 8 []) @ (from 12 20 []) @ (from 22 43 []) @
            (from 45 52 []) @ (from 57 63 []) in
  List.filter (fun x -> f x st col) lst

(* for buildings, consider:
 * 1. how many VP the building is worth
 * 2. given current resources, figure out what to build or to wait
 * 3. by building it, what other building options for the future becomes avaiable *)
let init_choose_settlement_build st = failwith "TODO"

let init_choose_road_build = failwith "TODO"

let make_build_plan = failwith "TODO"

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
