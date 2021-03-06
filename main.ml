open State
open Command
open Gui
open Elements
open Player
open Ai

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

let rec discard n s =
  if n >= 4 then s
  else
    let color = (List.nth s.players n).color in
    if num_all_resources color s <= 7 then discard (n + 1) s
    else if color <> Red then
      let () = print_string (string_of_color color ^ " was robbed.\n") in
      let () =
        if n >= 3 || (List.nth s.players (n + 1)).color <> Red then ()
        else print_newline ()
      in
      let to_discard = choose_discards color s in
      s |> do_move (Discard to_discard) (Some color) |> discard (n + 1)
    else
      let rec count acc = function
        | [] -> acc
        | (_, n) :: t -> count (acc + n) t
      in
      let x = num_all_resources color s / 2 in
      let msg = "Please discard " ^ string_of_int x ^ " resources.\n" in
      ANSITerminal.(print_string [cyan] msg);
      print_string  "> ";
      let to_discard = extract () in
      if count 0 to_discard < x then
        let () =
          if count 0 to_discard = 0 then print_newline ()
          else print_endline "Please specify more resources.\n"
        in
        discard n s
      else
        let sx = do_move (Discard to_discard) (Some Red) s in
        if sx = s then
          let () = print_endline "I am afraid I can't do that.\n" in
          discard n s
        else
          let () = update_canvas sx in
          let () = print_endline "Ok.\n" in
          discard (n + 1) sx

let rec robber s =
  if s.turn = Red then
    let () = ANSITerminal.(print_string [cyan] "Please move the robber.") in
    let () = print_newline () in
    match () |> parse_mouse_click |> nearby_tile s.canvas.tiles with
    | None -> print_newline (); robber s
    | Some i ->
      let sx = do_move (Robber i) None s in
      if sx <> s then
        begin
          print_endline "Ok.\n";
          draw_robber s.robber sx;
          update_canvas sx;
          sx
        end
      else let () = print_endline "I am afraid I cannot do that.\n" in robber s
  else
    let sx = do_move (Robber (choose_robber_spot s.turn s)) None s in
    if sx = s then ()
    else
      begin
        draw_robber s.robber sx;
        update_canvas sx;
        print_endline (string_of_color s.turn ^ " has moved the robber.")
      end;
    sx

let roll_dice s =
  let d1 = 1 + Random.int 6 in
  let d2 = 1 + Random.int 6 in
  let msg =
    if s.turn = Red then
      "It's your turn. You have rolled a "
      ^ string_of_int (d1 + d2) ^ "."
    else
      let name = string_of_color s.turn in
      "It's " ^ name ^ "'s" ^ " turn. " ^ name ^ " has rolled a "
      ^ string_of_int (d1 + d2) ^ "."
  in
  ANSITerminal.(print_string [on_cyan] msg);
  let () = print_newline () in
  let () = update_dice d1 d2 in
  let sx =
    if d1 + d2 <> 7 then generate_resource (d1 + d2) s
    else s |> discard 0 |> robber
  in
  sx

let trade to_remove to_add s =
  if s.turn <> Red then
    begin
      let f acc (r, n) = acc ^ string_of_int n ^ " " ^ string_of_resource r in
      let msg = string_of_color s.turn ^ " wants to trade "
                ^ List.fold_left f "" to_remove
                ^ " for "
                ^ List.fold_left f "" to_add
                ^ ".\n"
      in
      ANSITerminal.(print_string [white] msg);
      match
        List.fold_left (
          fun acc x ->
            if acc <> None then acc
            else if x.color = Red && trade_ok to_remove to_add (Some Red) s then
              let msg = "\nWould you like to trade with "
                        ^ string_of_color s.turn
                        ^ "?\n" in
              ANSITerminal.(print_string [cyan] msg);
              print_string "> ";
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
        let msg = string_of_color s.turn ^ " cannot find a trade partner." in
        print_endline msg; None
      | Some color ->
        let () =
          if color = Red then ()
          else
            let msg = string_of_color s.turn ^ " has traded with "
                      ^ string_of_color color ^ "." in
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

let rec repl (turns : int) (cmd : command) (clr_opt : color option) (s : state) =
  let tmp = do_move cmd clr_opt s in
  if tmp.turn <> s.turn && s.turn = Red then print_endline "Ok.\n" else ();
  if check_win tmp.turn tmp then
    let msg =
      "\nCongratulations! "
      ^ (if tmp.turn = Red then "You have"
         else string_of_color tmp.turn ^ " has")
      ^ " won the Settlers of Clarktan."
    in
    let () = print_endline msg in
    raise Exit
  else ();
  let sx = if s.turn = tmp.turn && cmd <> Start then tmp else roll_dice tmp in
  if sx <> s then update_canvas sx else ();
  if sx.turn <> Red then
    let () =
      if s <> sx then
        let name = string_of_color sx.turn in
        match cmd with
        | InitSettlement _ | InitRoad _ | Quit -> failwith "Impossible."
        | BuildSettlement _ -> print_endline (name ^ " has built a settlement.")
        | BuildRoad _ -> print_endline (name ^ " has built a road.")
        | BuildCity _ -> print_endline (name ^ " has built a city.")
        | BuyCard -> print_endline (name ^ " has bought a card.")
        | PlayKnight _ ->
          print_endline (name ^ " has played a Knight card.");
          draw_robber s.robber sx;
          update_canvas sx
        | PlayRoadBuilding _ ->
          print_endline (name ^ " has played a Road Building card.");
        | PlayMonopoly _ ->
          print_endline (name ^ " has played a Monopoly card.");
        | PlayYearOfPlenty _ ->
          print_endline (name ^ " has played a Year of Plenty card.");
        | _ -> ()
      else ()
    in
    let cmdx = if turns > 25 then EndTurn else choose sx.turn sx in
    match cmdx with
    | DomesticTrade (l0, l1) -> repl (turns + 1) cmdx (trade l0 l1 sx) sx
    | EndTurn ->
      let () =
        match cmd with
        | DomesticTrade _ -> if clr_opt = Some Red then () else print_newline ()
        | _ -> print_newline ()
      in
      repl 0 EndTurn None sx
    | _ -> repl (turns + 1) cmdx None sx
  else begin
    begin
      match cmd with
      | Start | EndTurn -> ()
      | InitSettlement _ | InitRoad _ -> failwith "Impossible."
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
      | PlayKnight i ->
        if s <> sx then
          let () = draw_robber s.robber sx in
          print_endline "Ok.\n"
        else print_endline "I am afraid I cannot do that.\n";
      | _ ->
        if s <> sx then print_endline "Ok.\n"
        else print_endline "I am afraid I cannot do that.\n"
    end;
    let prompt = "Please enter a command." in
    ANSITerminal.(print_string [cyan] prompt);
    print_newline ();
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
