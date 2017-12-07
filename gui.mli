(* GUI module handles the GUI and user interface for the game *)

(* [init_welcome f] draws the welcome page of the catan game. *)
val init_welcome : unit -> unit

(* [draw_canvas st] draws the canvas background for the game
 * under current state [st]. *)
val draw_canvas : State.state -> unit

(* [update_canvas st] updates the non-static part of canvas for the game
 * under current state [st]. *)
val update_canvas : State.state -> unit

(* [update_dice i1 i2] updates the dice image of canvas for the game
 * according to the two dice numbers [i1] [i2], obtainable from the
 * roll_dice function in main as a tuple. *)
val update_dice : int -> int -> unit

(* [draw_robber i st] updates the robber of canvas. *)
val draw_robber : int -> State.state -> unit
