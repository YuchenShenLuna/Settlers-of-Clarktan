open State

let roll_dice () =
  let i1 = 1 + Random.int 6 in
  let i2 = 1 + Random.int 6 in
  i1 + i2

let repl () = failwith ""

let main f =
  Graphics.open_graph " 1000x750";
  ANSITerminal.(
    print_string [red] "\n\nWelcome to the Settlers of Clarktan.\n\n"
  );
  print_string "> What would you like to do?\n>";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> () |> init_state |> Gui.draw_canvas

let () = main ()
