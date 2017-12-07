(* [repl ()] runs the REPL. *)
val repl : int -> Command.command -> Elements.color option -> State.state -> unit

(* [main ()] starts the REPL. *)
val main : unit -> unit
