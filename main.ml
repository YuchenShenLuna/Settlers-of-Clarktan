open State

let roll_dice () =
  let i1 = 1 + Random.int 6 in
  let i2 = 1 + Random.int 6 in
  i1+i2

(* let check_player = failwith "TODO" *)

let repl = let _ = init_canvas () in Gui.draw_canvas ()

let play f =
    Graphics.open_graph " 1000x750";
    ANSITerminal.(print_string [red]
                    "\n\nWelcome to Settlers of Clarktan.\n");
    print_string  "> Please enter what do you want to do next?";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> repl

let () = play ()
