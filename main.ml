open State
open Graphics

let roll_dice () =
  let i1 = 1 + Random.int 6 in
  let i2 = 1 + Random.int 6 in
  i1 + i2

let repl () = failwith ""

let rec main f =
  let state = init_state () in
  open_graph " 1000x750";
  set_window_title "Clarktan";
  Gui.draw_canvas state;
  ANSITerminal.(
    print_string [red] "\n\nWelcome to the Settlers of Clarktan.\n\n"
  );
  read_line (); ()

let () = main ()
