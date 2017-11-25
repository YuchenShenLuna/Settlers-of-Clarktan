(* Command module takes in player's command, either through mouse click
 * or terminal entry, and parses them accordingly. *)

(* command represents possible types of commands *)
type command

(* [parse str] parses the command given by [str] to type command *)
val parse_text : State.state -> string -> command
