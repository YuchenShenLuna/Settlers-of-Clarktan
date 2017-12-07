open State
open Command
open Gui
open Elements
open Player
open Ai

let string_of_color = function
  | Red -> "Red"
  | Blue -> "Blue"
  | Green -> "Green"
  | Yellow -> "Yellow"

let setup s =
  let rec settlement s =
    ANSITerminal.(print_string [cyan] "Please pick a settlement.");
    print_newline ();
    match () |> parse_mouse_click |> nearby_intersection s.canvas.tiles with
    | None -> print_endline "I am afraid I cannot do that.\n"; settlement s
    | Some i ->
      let sx = do_move (InitSettlement i) None s in
      if s = sx then
        begin
          print_endline "I am afraid I cannot do that.\n";
          settlement s
        end
      else let () = print_endline "Ok.\n" in sx
  in
  let rec road s =
    ANSITerminal.(print_string [cyan] "Please pick a road.");
    print_newline ();
    match () |> parse_mouse_click |> nearby_edge s.canvas.tiles with
    | None -> print_endline "I am afraid I cannot do that.\n"; road s
    | Some i ->
      let sx = do_move (InitRoad i) None s in
      if s = sx then
        begin
          print_endline "I am afraid I cannot do that.\n";
          road s
        end
      else let () = print_endline "Ok.\n" in sx
  in
  let rec helper n s =
    let () = update_canvas s in
    if n = 8 then s
    else if n < 4 then
      let sx =
        if s.turn = Red then
          let tmp = settlement s in
          let () = update_canvas tmp in
          tmp |> road
        else
          let i = first_settlement s s.turn in
          let tmp = s |> do_move (InitSettlement i) None in
          let r = init_road tmp s.turn i in
          tmp |> do_move (InitRoad r) None
      in
      if n <> 3 then sx |> end_turn true |> helper (n + 1)
      else sx |> helper (n + 1)
    else
      let sx =
        begin
          if s.turn = Red then
            let tmp = settlement s in
            let () = update_canvas tmp in
            tmp |> road
          else
            let i = second_settlement s s.turn in
            let tmp = s |> do_move (InitSettlement i) None in
            let r = init_road tmp s.turn i in
            tmp |> do_move (InitRoad r) None
        end
        |> init_generate_resources s.turn
      in
      if n <> 7 then sx |> end_turn false |> helper (n + 1)
      else sx |> helper (n + 1)
  in
  s |> helper 0

let robber s = s (* TODO *)

let roll_dice s =
  let d1 = 1 + Random.int 6 in
  let d2 = 1 + Random.int 6 in
  let sx = if d1 + d2 <> 7 then generate_resource (d1 + d2) s else robber s in
  let msg =
    if s.turn = Red then
      "It's your turn. You have rolled a "
      ^ string_of_int (d1 + d2) ^ ". "
    else
      let name = string_of_color s.turn in
      "It's " ^ name ^ "'s" ^ " turn. " ^ name ^ " has rolled a "
      ^ string_of_int (d1 + d2) ^ ".\n\n"
  in
  ANSITerminal.(print_string [cyan] msg);
  print_string "";
  update_dice d1 d2;
  sx

let trade to_remove to_add s =
  print_newline ();
  match
    List.fold_left (
      fun acc x ->
        if acc <> None then acc
        else if x.color <> s.turn
             && want_accept_trade s x.color to_add to_remove then
          let msg = "Would you like to trade with "
                    ^ string_of_color x.color
                    ^ "?" in
          print_endline msg;
          let color_opt = if feedback () |> not then None else Some x.color in
          print_endline "Ok.\n";
          color_opt
        else None
    ) None s.players
  with
  | None -> let () = print_endline "No one wants to trade with you :(\n" in None
  | Some color -> Some color

let rec repl (cmd : command) (clr_opt : color option) (s : state) =
  let tmp = do_move cmd clr_opt s in
  if tmp <> s && s.turn = Red then print_endline "Ok.\n" else ();
  let _ = print_endline (string_of_command cmd); print_newline () in
  let sx = if s.turn = tmp.turn && cmd <> Start then tmp else roll_dice tmp in
  if sx <> s then update_canvas sx else ();
  if sx.turn <> Red then repl (choose sx.turn sx) None sx
  else begin
    begin
      match cmd with
      | Start | EndTurn -> ()
      | BuyCard ->
        let string_of_card = function
          | Knight -> "knight"
          | RoadBuilding -> "road building"
          | YearOfPlenty -> "year of plenty"
          | Monopoly -> "monopoly"
          | VictoryPoint -> "victory point"
        in
        if s <> sx then print_endline ("Ok. You have received a "
                                       ^ (s.deck |> List.hd |> string_of_card)
                                       ^ " card.\n")
        else print_endline "I am afraid I cannot do that.\n"
      | DomesticTrade (lst0, lst1) ->
        if s <> sx || clr_opt = None then ()
        else print_endline "I am afraid I cannot do that.\n"
      | Quit -> print_endline "Goodbye.\n"; raise Exit
      | Invalid -> print_endline "I do not understand.\n"
      | _ ->
        if s <> sx then print_endline "Ok.\n"
        else print_endline "I am afraid I cannot do that.\n"
    end;
    let prompt = "Please enter a command.\n" in
    ANSITerminal.(print_string [cyan] prompt);
    print_string  "> ";
    let cmdx =
      match read_line () with
      | exception End_of_file -> Invalid
      | str -> Command.parse_text s.canvas.tiles str
    in
    match cmdx with
    | DomesticTrade (l0, l1) ->
      repl cmdx (trade l0 l1 sx) sx
    | _ -> repl cmdx None sx
  end

let main () =
  let () = Random.self_init () in
  let s = init_state () in
  Graphics.open_graph " 1000x750";
  Graphics.set_window_title "Settlers of Clarktan by Yuchen Shen, Yishu Zhang, \
                             Esther Jun";
  draw_canvas s;
  let _ = Sys.command("clear") in
  ANSITerminal.(print_string [red] "Welcome to the Settlers of Clarktan.");
  print_newline ();
  match s |> setup |> repl Start None with
  | exception Exit -> Graphics.close_graph ()
  | _ -> print_endline "Oh no! Something went wrong."

let () = main ()
