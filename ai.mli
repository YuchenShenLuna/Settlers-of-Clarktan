open Elements
open State

(* [first_settlement st col] returns the index of the first
 * settlement chosen by the ai at initial building phase under state [st]. The
 * player is identified by color [col].
 * Checks: 1. dice num with higher prob
           2. covers more resources with good priority *)
val first_settlement : state -> color -> intersection

(* [second_settlement st col] returns the index of the second
 * settlement chosen by the ai at initial building phase under state [st]. The
 * player is identified by color [col].
 * This depends on what resource it does not have access to,
 * and dice num probability *)
val second_settlement : state -> color -> intersection

(* [init_road_build st col ind] returns the index of the road to be build
 * by the ai at initial phase *)
val init_road : state -> color -> intersection -> edge

val choose : color -> state -> Command.command
