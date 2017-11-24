open State

open Player

open Command

(* Main module makes the repl for the game to start and play *)

(* [roll_dice] generates a sum of randomly rolling two dices *)
val roll_dice : unit -> int

(* [check_player] checks which player's turn it is *)
val check_player : player -> player

(* [repl st] is the repl for the game *)
val repl : state -> state

(* [play] controls the gameplay *)
val play: unit -> unit
