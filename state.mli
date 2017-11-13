open Player

open Canvas

open Tile

open DevCard

type state

val fetch_tiles : int -> tile list

val roll_dice : unit -> int

val next_turn : state -> state

val play_devcard : devcard -> state -> state

val move_robber : state -> state

val build_building : state -> state

val build_road : state -> state

val trade : state -> state

val check_build_building : state -> bool

val check_build_road : state -> bool

val do_player : state -> state

val do_ai : state -> state
