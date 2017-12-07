open State
open Command
open Gui
open Elements
open Player
open Ai

(* [setup s] completes the setting up stage for the game play. *)
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
  if s.turn <> Red then
    begin
      let g (r, n) = print_string (string_of_int n ^ " " ^ string_of_resource r) in
      print_string (string_of_color s.turn ^ " would like to trade ");
      List.iter g to_remove;
      print_string " for ";
      List.iter g to_add;
      print_string ". ";
      match
        List.fold_left (
          fun acc x ->
            if acc <> None then acc
            else if x.color = Red then
              let msg = "Would you like to trade with "
                        ^ string_of_color s.turn
                        ^ "?\n> " in
              print_string msg;
              let color_opt = if feedback () |> not then None else Some Red in
              print_endline "Ok.\n";
              color_opt
            else if x.color <> s.turn
                 && want_accept_trade to_add to_remove x.color s then
              Some x.color
            else None
        ) None s.players
      with
      | None ->
        let msg = string_of_color s.turn ^ " cannot find a trade partner.\n" in
        print_endline msg; None
      | Some color ->
        let () =
          if color = Red then ()
          else
            let msg = string_of_color s.turn ^ " has traded with "
                      ^ string_of_color color ^ ".\n" in
            print_endline msg
        in
        Some color
    end
  else
    let () = print_newline () in
    match
      List.fold_left (
        fun acc x ->
          if acc <> None then acc
          else if x.color <> s.turn
               && want_accept_trade to_add to_remove x.color s then
            let msg = "Would you like to trade with "
                      ^ string_of_color x.color
                      ^ "?\n> " in
            print_string msg;
            let color_opt = if feedback () |> not then None else Some x.color in
            print_endline "Ok.\n";
            color_opt
          else None
      ) None s.players
    with
    | None -> print_endline "No one wants to trade with you :(\n"; None
    | Some color -> Some color

let game_over s =
  List.iter (
    fun x ->
      if check_win x.color s then
        let msg =
          "Congratulations! "
          ^ (if x.color = Red then "You have"
             else string_of_color x.color ^ " has")
          ^ " won the Settlers of Clarktan."
        in
        print_endline msg
      else ()
  ) s.players

let rec repl (turns : int) (cmd : command) (clr_opt : color option) (s : state) =
  let tmp = do_move cmd clr_opt s in
  if tmp.turn <> s.turn && s.turn = Red then print_endline "Ok.\n" else ();
  let _ = print_endline (string_of_command cmd); print_newline () in
  let () = game_over s in
  let sx = if s.turn = tmp.turn && cmd <> Start then tmp else roll_dice tmp in
  if sx <> s then update_canvas sx else ();
  if sx.turn <> Red then
    begin
      begin
        match cmd with
        | PlayKnight i | Robber i ->
          if s <> sx then draw_robber s.robber sx else ()
        | _ -> ()
      end;
      let cmdx = if turns > 25 then EndTurn else choose sx.turn sx in
      match cmdx with
      | DomesticTrade (l0, l1) -> repl (turns + 1) cmdx (trade l0 l1 sx) sx
      | EndTurn -> repl 0 EndTurn None sx
      | _ -> repl (turns + 1) cmdx None sx
    end
  else begin
    begin
      match cmd with
      | Start | EndTurn -> ()
      | BuyCard ->
        if s <> sx then print_endline ("Ok. You have received a "
                                       ^ (s.deck |> List.hd |> string_of_card)
                                       ^ " card.\n")
        else print_endline "I am afraid I cannot do that.\n"
      | DomesticTrade (lst0, lst1) ->
        if s <> sx || clr_opt = None then ()
        else print_endline "I am afraid I cannot do that.\n"
      | Quit -> print_endline "Goodbye.\n"; raise Exit
      | Invalid -> print_endline "I do not understand.\n"
      | PlayKnight i | Robber i ->
        if s <> sx then
          let () = draw_robber s.robber sx in
          print_endline "Ok.\n"
        else print_endline "I am afraid I cannot do that.\n";
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
    | DomesticTrade (l0, l1) -> repl (turns + 1) cmdx (trade l0 l1 sx) sx
    | EndTurn -> repl 0 EndTurn None sx
    | _ -> repl (turns + 1) cmdx None sx
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
  match s |> setup |> repl 0 Start None with
  | exception Exit -> Graphics.close_graph ()
  | _ -> print_endline "Oh no! Something went wrong."

let () = main ()
