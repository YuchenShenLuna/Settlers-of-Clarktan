open Canvas

open State

(* GUI module handles the GUI and user interface for the game *)

(* [draw_canvas] draws the canvas background for the game *)
val draw_canvas : unit -> unit

(* [draw_robber] draws the robber onto the canvas of the game *)
val draw_robber : unit -> unit

(* [update_player_info] draws player informations in the game *)
val update_player_info : unit -> unit