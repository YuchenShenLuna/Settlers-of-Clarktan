open OUnit2
open State
open Command
open Elements

(* card int list no dups no zeros *)
let cmp l0 l1 =
  List.length l0 = List.length l1
  && List.fold_left (fun acc x -> List.assoc (fst x) l1 = snd x && acc) true l0

let card_tests =[
  "init_score" >:: (fun _ -> assert_equal 0 (state_to_test |> score Red));
  "init_resources_red" >:: (fun _ -> assert_equal 25
                           (state_to_test |> num_all_resources Red));
  "init_resources_green" >:: (fun _ -> assert_equal 25
                                 (state_to_test |> num_all_resources Green));
  "init_resources_blue" >:: (fun _ -> assert_equal 25
                                 (state_to_test |> num_all_resources Blue));
  "init_resources_yellow" >:: (fun _ -> assert_equal 25
                                 (state_to_test |> num_all_resources Yellow));
  "init_cards" >:: (fun _ -> assert_equal [] (state_to_test |> cards Red));
  "init_resource_number_wool" >::
    (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Wool));
  "init_resource_number_lumber" >::
    (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Lumber));
  "init_resource_number_grain" >::
    (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Grain));
  "init_resource_number_brick" >::
    (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Brick));
  "init_resource_number_ore" >::
    (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Ore));
  "buy_card" >::
  (fun _ -> assert_equal ~cmp [YearOfPlenty, 1]
      (state_to_test
       |> do_move BuyCard (Some Red)
       |> cards Red));
  "resource_quantity_buy_card_ore" >::
  (fun _ -> assert_equal 4 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> num_resource Red Ore));
  "resource_quantity_buy_card_wool" >::
  (fun _ -> assert_equal 4 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> num_resource Red Wool));
  "resource_quantity_buy_card_grain" >::
  (fun _ -> assert_equal 4 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> num_resource Red Grain));
  "resource_quantity_buy_card_lumber" >::
  (fun _ -> assert_equal 5 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> num_resource Red Lumber));
  "resource_quantity_buy_card_brick" >::
  (fun _ -> assert_equal 5 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> num_resource Red Brick));
  (*"resource_quantity_buy_card_brick" >:: (fun _ -> assert_equal
                                              12
          (state_to_test |> do_move BuyCard (Some Red)|> num_all_resources Red));*)
  "buy_card_again" >::
  (fun _ -> assert_equal ~cmp [(YearOfPlenty, 1);(VictoryPoint,1)]
      (state_to_test
       |> do_move BuyCard (Some Red)
       |>do_move BuyCard (Some Red)
       |> cards Red));
(*"resource_quantity_buy_card" >:: (fun _ -> assert_equal
                                     9
(state_to_test |> do_move BuyCard (Some Red)
|>do_move BuyCard (Some Red)|> num_all_resources Red));
"check_turn" >:: (fun _ -> assert_equal
              Red
               (state_to_test |>
             do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> turn));*)
  "check_turn_after_end_turn">::
  (fun _ -> assert_equal Yellow (state_to_test
                                 |> do_move BuyCard (Some Red)
                                 |> do_move BuyCard (Some Red)
                                 |> end_turn true|>turn));
  "another_player_buy_card">::
  (fun _ -> assert_equal ~cmp [(Knight, 1)]
      (state_to_test
       |> do_move BuyCard (Some Red)
       |> do_move BuyCard (Some Red)
       |> end_turn true
       |> do_move BuyCard (Some Yellow)
       |>cards Yellow));
  "resource_quantity_buy_card_ore_yellow" >::
  (fun _ -> assert_equal 4 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> do_move BuyCard (Some Red)
                            |> end_turn true
                            |> do_move BuyCard (Some Yellow)
                            |> num_resource Yellow Ore));
  "resource_quantity_buy_card_wool_yellow" >::
  (fun _ -> assert_equal 4 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> do_move BuyCard (Some Red)
                            |> end_turn true
                            |> do_move BuyCard (Some Yellow)
                            |> num_resource Yellow Wool));
  "resource_quantity_buy_card_grain_yellow" >::
  (fun _ -> assert_equal 4 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> do_move BuyCard (Some Red)
                            |> end_turn true|>do_move BuyCard (Some Yellow)
                            |> num_resource Yellow Grain));
  "resource_quantity_buy_card_brick_yellow" >::
  (fun _ -> assert_equal 5 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> do_move BuyCard (Some Red)
                            |> end_turn true
                            |> do_move BuyCard (Some Yellow)
                            |> num_resource Yellow Brick));
  "resource_quantity_buy_card_lumber_yellow" >::
  (fun _ -> assert_equal 5 (state_to_test
                            |> do_move BuyCard (Some Red)
                            |> do_move BuyCard (Some Red)
                            |> end_turn true
                            |> generate_resource 3
                            |> do_move BuyCard (Some Yellow)
                            |> num_resource Yellow Lumber));
  "blue_turn" >::
  (fun _ -> assert_equal Blue (state_to_test
                               |> do_move BuyCard (Some Red)
                               |>do_move BuyCard (Some Red)
                               |> end_turn true
                               |> generate_resource 3
                               |> do_move BuyCard (Some Yellow)
                               |> end_turn true|>turn ));
  (*"blue_turn_trade_with_bank" >::*)
  (*(fun _ ->
     assert_equal  1 (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|> end_turn true|>do_move (MaritimeTrade (false,(Wool,4),(Ore,1))) (Some Blue))|>num_resource Blue Ore)));*)

  "blue_turn_trade_with_bank_checking_lumber" >::
  (fun _ -> assert_equal 5
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |>num_resource Blue Lumber));
  "blue_turn_trade_with_bank_checking_wool" >::
  (fun _ -> assert_equal 1
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |>num_resource Blue Wool));
  "blue_turn_trade_with_bank_checking_ore" >::
  (fun _ -> assert_equal 6
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |>num_resource Blue Ore));
  "blue_turn_trade_with_bank_checking_brick" >::
      (fun _ -> assert_equal 5
          (state_to_test
          |> do_move BuyCard (Some Red)
          |> do_move BuyCard (Some Red)
          |> end_turn true
          |> generate_resource 3
          |> do_move BuyCard (Some Yellow)
          |> end_turn true
          |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
          |>num_resource Blue Brick));
  "blue_turn_trade_with_bank_checking_grain" >::
      (fun _ -> assert_equal 5
        (state_to_test
          |> do_move BuyCard (Some Red)
          |> do_move BuyCard (Some Red)
          |> end_turn true
          |> generate_resource 3
          |> do_move BuyCard (Some Yellow)
          |> end_turn true
          |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
          |>num_resource Blue Grain));
  "blue_turn_trade_with_bank_checking_grain" >::
      (fun _ -> assert_equal 5
         (state_to_test
          |> do_move BuyCard (Some Red)
          |> do_move BuyCard (Some Red)
          |> end_turn true
          |> generate_resource 3
          |> do_move BuyCard (Some Yellow)
          |> end_turn true
          |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
          |>num_resource Blue Lumber));
  "green_turn_trade_build_settlement_near_port" >::
  (fun _ -> assert_equal 1
      (state_to_test
      |> do_move BuyCard (Some Red)
      |>do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |>end_turn true
      |>do_move (BuildSettlement 5) (Some Green)
      |>score Green));
  "green_turn_trade_build_settlement_near_port_turn_check" >::
  (fun _ -> assert_equal Green
      (state_to_test
      |> do_move BuyCard (Some Red)
      |>do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |>end_turn true
      |>do_move (BuildSettlement 5) (Some Green)
      |>turn));
  "green_turn_trade_with_port" >::
  (fun _ -> assert_equal 0
      (state_to_test
      |> do_move BuyCard (Some Red)
      |>do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |>num_resource Green Brick));
  "green_turn_trade_with_port" >::
  (fun _ -> assert_equal 0
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> num_resource Green Brick));
  "green_turn_trade_with_port" >::
  (fun _ -> assert_equal 6
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> num_resource Green Wool));
  "green_turn_trade_with_port_grain" >::
  (fun _ -> assert_equal 5
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> num_resource Green Grain));
  "turn_update_green" >::
  (fun _ -> assert_equal Green
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> turn));
  "turn_update_green_then_marime_trade_then_check_ore_with_gerate_resource" >::
  (fun _ -> assert_equal 5
      (state_to_test
      |> do_move BuyCard (Some Red)
      |>do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 8
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> generate_resource 8
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> num_resource Green Ore));
  "back_again_red" >::
  (fun _ -> assert_equal Red
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> end_turn true
      |> turn));
  "red_with_victory_point_Card_" >::
  (fun _ -> assert_equal 1
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> end_turn true
      |> score Red));
  "turn_update_green_then_marime_trade_then_check_grain" >::
  (fun _ -> assert_equal 5
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 8
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> generate_resource 3
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> num_resource Green Grain));
  "red_play_victory_point_ore" >::
  (fun _ -> assert_equal 4
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> end_turn true
      |> do_move (PlayYearOfPlenty (Ore,Wool)) (Some Red)
      |> num_resource Red Ore));
  "red_play_victory_point_wool" >::
  (fun _ -> assert_equal 4
      (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> end_turn true
      |> do_move (PlayYearOfPlenty (Ore,Wool)) (Some Red)
      |> num_resource Red Wool));
  "red_turn" >::
      (fun _ -> assert_equal Red
          (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> end_turn true
      |> do_move (PlayYearOfPlenty (Ore,Wool)) (Some Red)
      |> turn));
  "resource_quantity_buy_card_brick" >::
      (fun _ -> assert_equal  22
          (state_to_test
      |> do_move BuyCard (Some Red)
      |> num_all_resources Red));
  "resource_quantity_buy_card" >::
      (fun _ -> assert_equal 19
          (state_to_test
      |> do_move BuyCard (Some Red)
      |>do_move BuyCard (Some Red)
      |> num_all_resources Red));
  "yellow_score" >::
           (fun _ -> assert_equal 0
               (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> end_turn true
      |> do_move (PlayYearOfPlenty (Ore,Wool)) (Some Red)
      |> score Yellow));
  "green_score" >::
              (fun _ -> assert_equal 0
                (state_to_test
      |> do_move BuyCard (Some Red)
      |> do_move BuyCard (Some Red)
      |> end_turn true
      |> generate_resource 3
      |> do_move BuyCard (Some Yellow)
      |> end_turn true
      |> do_move (MaritimeTrade ((Wool,4),(Ore,1))) (Some Blue)
      |> end_turn true
      |> do_move (BuildSettlement 5) (Some Green)
      |> do_move (MaritimeTrade ((Brick,4),(Wool,2))) (Some Green)
      |> end_turn true
      |> do_move (PlayYearOfPlenty (Ore,Wool)) (Some Red)
      |> score Blue));
]

let suite = "" >:::  card_tests

let _ = run_test_tt_main suite
