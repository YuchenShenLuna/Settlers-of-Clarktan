(* [roll_dice ()] is the sum of two dice rolls. *)
val roll_dice : unit -> int

(* [repl ()] runs the REPL. *)
val repl : Command.command -> Elements.color option -> State.state -> unit

(* [main ()] starts the REPL. *)
val main : unit -> unit
