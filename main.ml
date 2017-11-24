open State

open Player

open Command

let roll_dice = failwith "TODO"

let check_player = failwith "TODO"

let repl = failwith "TODO"

let play f =
    ANSITerminal.(print_string [red]
                    "\n\nWelcome to Settlers of Clarktan.\n");
    print_string  "> Please enter what do you want to do next?";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> repl

let () = play ()
