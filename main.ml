open State
open Command
open Gui
open Elements

let roll_dice () =
  let i1 = 1 + Random.int 6 in
  let i2 = 1 + Random.int 6 in
  i1 + i2

let rec repl (cmd : command) (clr_opt : color option) (s : state) =
  let sx = do_move cmd clr_opt s in
  begin
    match cmd with
    | Invalid -> print_endline "I do not understand."
    | Start -> ()
    | Setup (intersection, edge) -> failwith ""
    | BuildSettlement intersection -> failwith ""
    | BuildCity intersection -> failwith ""
    | BuildRoad edge -> failwith ""
    | BuyCard -> failwith ""
    | PlayKnight tile -> failwith ""
    | PlayRoadBuilding (edge0, edge1) -> failwith ""
    | PlayYearOfPlenty (resource0, resource1)-> failwith ""
    | PlayMonopoly resource -> failwith ""
    | Robber tile -> failwith ""
    | DomesticTrade (approved, lst0, lst1) -> failwith ""
    | MaritimeTrade (approved, lst0, lst1) -> failwith ""
    | Discard lst -> failwith ""
    | EndTurn -> failwith ""
    | Quit -> print_endline "Goodbye."; raise Exit
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
  print_newline ();
  repl cmdx None sx

let main () =
  let state = init_state () in
  Graphics.open_graph " 1000x750";
  Graphics.set_window_title "Clarktan";
  draw_canvas state;
  let _ = Sys.command("clear") in
  ANSITerminal.(print_string [red] "Welcome to the Settlers of Clarktan.");
  match () |> init_state |> repl Start None with
  | exception Exit -> Graphics.close_graph ()
  | _ -> print_endline "Oh no! Something went wrong."

let () = main ()
