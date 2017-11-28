open Elements

open State

let get_probability num =
  match num with
  | 2  -> (1, 36)
  | 3  -> (2, 36)
  | 4  -> (3, 36)
  | 5  -> (4, 36)
  | 6  -> (5, 36)
  | 7  -> (6, 36)
  | 8  -> (5, 36)
  | 9  -> (4, 36)
  | 10 -> (3, 36)
  | 11 -> (2, 36)
  | 12 -> (1, 36)
  | _  -> (0, 36)

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

let want_play_road_building = failwith "TODO"

let want_play_knight = failwith "TODO"

let choose_robber_spot = failwith "TODO"

(* if with a plan then keep resource for it else random *)
let choose_discard_resource = failwith "TODO"

let do_ai = failwith "TODO"
