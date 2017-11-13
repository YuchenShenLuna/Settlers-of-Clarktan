open Tile

open Building

open DevCard

(* Canvas module represents the canvas background of the game
 * the player will visualize. *)

(* representation type for canvas background *)
type canvas

(* [init_canvas] returns a new canvas to be used in a new game *)
val init_canvas : unit -> canvas
