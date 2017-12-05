open State
open Command
open Gui
open Elements
open Player
open Ai

let roll_dice () =
  let i1 = 1 + Random.int 6 in
  let i2 = 1 + Random.int 6 in
  (i1, i2), (i1 + i2)

let setup s =
  let rec settlement s =
    ANSITerminal.(print_string [cyan] "\nPlease pick a settlement.\n");
    match () |> parse_mouse_click |> nearby_intersection s.canvas.tiles with
    | None -> print_endline "I do not understand."; settlement s
    | Some i ->
      let sx = eval (InitSettlement i) None s in
      if s = sx then
        begin
          print_endline "I am afraid I cannot do that.";
          settlement s
        end
      else sx
  in
  let rec road s =
    ANSITerminal.(print_string [cyan] "\nPlease pick a road.\n");
    match () |> parse_mouse_click |> nearby_edge s.canvas.tiles with
    | None -> print_endline "I do not understand."; settlement s
    | Some i ->
      let sx = eval (InitRoad i) None s in
      if s = sx then
        begin
          print_endline "I am afraid I cannot do that.";
          settlement s
        end
      else sx
  in
  let rec helper n s =
    if n = 8 then s
    else if n < 4 then
      let sx =
        if s.turn = Red then s |> settlement |> road
        else
          let i = first_settlement s s.turn in
          let r = init_road s s.turn i in
          s |> eval (InitSettlement i) None |> eval (InitRoad r) None
      in
      if n <> 3 then sx |> end_turn false |> helper (n + 1)
      else sx |> helper (n + 1)
    else
      let sx =
        begin
          if s.turn = Red then s |> settlement |> road
          else
            let i = second_settlement s s.turn in
            let r = init_road s s.turn i in
            s |> eval (InitSettlement i) None |> eval (InitRoad r) None
        end
        |> init_generate_resources s.turn
      in
      let _ = update_canvas sx in
      sx |> end_turn false |> helper (n + 1)
  in
  s |> helper 0

let rec repl (cmd : command) (clr_opt : color option) (s : state) =
  let sw = eval cmd clr_opt s in
  let sx =
    if s.turn <> sw.turn then (roll_dice () |> snd |> generate_resource) sw
    else sw
  in
  begin
    match cmd with
    | Start -> ()
    | InitSettlement _ | InitRoad _ -> failwith "Impossible."
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
                                     ^ " card.")
      else print_endline "I am afraid I cannot do that."
    | DomesticTrade (approved, lst0, lst1) -> failwith ""
    | MaritimeTrade (approved, lst0, lst1) -> failwith ""
    | Quit -> print_endline "Goodbye."; raise Exit
    | Invalid -> print_endline "I do not understand."
    | _ ->
      if s <> sx then print_endline "Ok."
      else print_endline "I am afraid I cannot do that."
  end;
  print_newline ();
  let prompt = "Please enter a command.\n" in
  ANSITerminal.(print_string [cyan] prompt);
  print_string  "> ";
  let cmdx =
    match read_line () with
    | exception End_of_file -> Invalid
    | str -> Command.parse_text s.canvas.tiles str
  in
  repl cmdx None sx

let main () =
  let s = init_state () in
  Graphics.open_graph " 1000x750";
  Graphics.set_window_title "Settlers of Clarktan by Yuchen Shen, Yishu Zhang, \
                             Esther Jun";
  draw_canvas s;
  let _ = Sys.command("clear") in
  ANSITerminal.(print_string [red] "Welcome to the Settlers of Clarktan.\n");
  match s |> setup |> repl Start None with
  | exception Exit -> Graphics.close_graph ()
  | _ -> print_endline "Oh no! Something went wrong."

let () = main ()
