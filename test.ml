open OUnit2
open State
open Command
open Elements

let score_tests = [
  "init_score" >:: (fun _ -> assert_equal 0 (state_to_test |> score Red));
]

let resource_tests =[
  "init_resources" >:: (fun _ -> assert_equal 0
                          (state_to_test |> num_all_resources Red));
]

(* card int list no dups no zeros *)
let cmp l0 l1 =
  List.length l0 = List.length l1
  && List.fold_left (fun acc x -> List.assoc (fst x) l1 = snd x && acc) true l0

let card_tests =[
  "init_cards" >:: (fun _ -> assert_equal [] (state_to_test |> cards Red));
  "init_resource_number_wool" >:: (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Wool));
  "init_resource_number_lumber" >:: (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Lumber));
  "init_resource_number_grain" >:: (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Grain));
  "init_resource_number_brick" >:: (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Brick));
  "init_resource_number_ore" >:: (fun _ -> assert_equal 5 (state_to_test |> num_resource Red Ore));
  (*"init_resource_number_all" >:: (fun _ -> assert_equal 15 (state_to_test |> num_all_resources Red));*)
  "buy_card" >:: (fun _ -> assert_equal
                 ~cmp [Knight, 1]
                 (state_to_test |> do_move BuyCard (Some Red)|> cards Red));
  "resource_quantity_buy_card_ore" >:: (fun _ -> assert_equal
                                  4
        (state_to_test |> do_move BuyCard (Some Red)|> num_resource Red Ore));
  "resource_quantity_buy_card_wool" >:: (fun _ -> assert_equal
                                4
      (state_to_test |> do_move BuyCard (Some Red)|> num_resource Red Wool));
  "resource_quantity_buy_card_grain" >:: (fun _ -> assert_equal
                                                      4
        (state_to_test |> do_move BuyCard (Some Red)|> num_resource Red Grain));
  "resource_quantity_buy_card_lumber" >:: (fun _ -> assert_equal
                                                            5
        (state_to_test |> do_move BuyCard (Some Red)|> num_resource Red Lumber));
  "resource_quantity_buy_card_brick" >:: (fun _ -> assert_equal
                                          5
                                          (state_to_test |> do_move BuyCard (Some Red)|> num_resource Red Brick));
  (*"resource_quantity_buy_card_brick" >:: (fun _ -> assert_equal
                                              12
          (state_to_test |> do_move BuyCard (Some Red)|> num_all_resources Red));*)
  "buy_card_again" >:: (fun _ -> assert_equal
                         ~cmp [(Knight, 1);(VictoryPoint,1)]
            (state_to_test |>
         do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> cards Red));
(*"resource_quantity_buy_card" >:: (fun _ -> assert_equal
                                     9
(state_to_test |> do_move BuyCard (Some Red)
|>do_move BuyCard (Some Red)|> num_all_resources Red));
"check_turn" >:: (fun _ -> assert_equal
              Red
               (state_to_test |>
             do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> turn));*)
 "check_turn_after_end_turn">:: (fun _ -> assert_equal
        Yellow
        (state_to_test |>
do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>turn));
 "another_player_buy_card">:: (fun _ -> assert_equal
        ~cmp [(Knight, 1)]
    (state_to_test |>
     do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>do_move BuyCard (Some Yellow)
     |>cards Yellow));
  "resource_quantity_buy_card_ore_yellow" >:: (fun _ -> assert_equal
                                     4
                                     (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>do_move BuyCard (Some Yellow)|> num_resource Yellow Ore));
  "resource_quantity_buy_card_wool_yellow" >:: (fun _ -> assert_equal
                                                                     4
  (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>do_move BuyCard (Some Yellow)|> num_resource Yellow Wool));

  "resource_quantity_buy_card_grain_yellow" >:: (fun _ -> assert_equal
                              4
                              (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>do_move BuyCard (Some Yellow)|> num_resource Yellow Grain));
  "resource_quantity_buy_card_brick_yellow" >:: (fun _ -> assert_equal
                                                        5
                                                        (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>do_move BuyCard (Some Yellow)|> num_resource Yellow Brick));
  "resource_quantity_buy_card_lumber_yellow" >:: (fun _ -> assert_equal
                                                      5
                                                      (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|> num_resource Yellow Lumber));
  "blue_turn" >:: (fun _ -> assert_equal
                  Blue
  (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|> end_turn true|>turn ));
  (*"blue_turn_trade_with_bank" >::*)
  (*(fun _ ->
     assert_equal  1 (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|> end_turn true|>do_move (MaritimeTrade (false,(Wool,4),(Ore,1))) (Some Blue))|>num_resource Blue Ore)));*)

     "blue_turn_trade_with_bank_checking_lumber" >:: (fun _ -> assert_equal
                                                         5
                                                         (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|>end_turn true|>do_move (MaritimeTrade (true,(Wool,4),(Ore,1))) (Some Blue)
                                                         |>num_resource Blue Lumber));

    "blue_turn_trade_with_bank_checking_wool" >:: (fun _ -> assert_equal
                                                      1
                                                  (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|>end_turn true|>do_move (MaritimeTrade (true,(Wool,4),(Ore,1))) (Some Blue)
                                                  |>num_resource Blue Wool));
  "blue_turn_trade_with_bank_checking_ore" >:: (fun _ -> assert_equal
                                                    6
                        (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|>end_turn true|>do_move (MaritimeTrade (true,(Wool,4),(Ore,1))) (Some Blue)
                         |>num_resource Blue Ore));

 "green_turn_trade_build_settlement_near_port" >:: (fun _ -> assert_equal
                            1
  (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|>end_turn true|>do_move (MaritimeTrade (true,(Wool,4),(Ore,1))) (Some Blue)
   |>end_turn true|>do_move (BuildSettlement 5) (Some Green)|>score Green));

"green_turn_trade_with_port" >:: (fun _ -> assert_equal
                              2
  (state_to_test |> do_move BuyCard (Some Red)|>do_move BuyCard (Some Red)|> end_turn true|>generate_resource 3|>do_move BuyCard (Some Yellow)|>end_turn true|>do_move (MaritimeTrade (true,(Wool,4),(Ore,1))) (Some Blue)
   |>end_turn true|>do_move (BuildSettlement 5) (Some Green)|>do_move (MaritimeTrade (true,(Brick,2),(Wool,1))) (Some Green)|>num_resource Green Brick));

]



(*let structure_tests =[
  "init_score" >:: (fun _ -> assert_equal 0 (state_to_test |> score Red));
]

let trade_tests =[
  "init_score" >:: (fun _ -> assert_equal 0 (state_to_test |> score Red));
]

let robber_tests =[
  "init_score" >:: (fun _ -> assert_equal 0 (state_to_test |> score Red));
]

let helper_tests =[

  ]*)


let suite = "" >::: score_tests @ card_tests

let _ = run_test_tt_main suite
