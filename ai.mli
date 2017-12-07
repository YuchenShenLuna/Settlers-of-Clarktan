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

(* [discard_resource col s] discards resource for player with color [col]. *)
val discard_resources : color -> state -> state

(* [want_accept_trade l1 l2 c s] returns whether player with color [c]
 * wants to accept the trade. *)
val want_accept_trade : (resource * int) list -> (resource * int) list -> color -> state -> bool

(* [choose_robber_spot c s] chooses the robber index for player [c]. *)
val choose_robber_spot : color -> state -> int

(* [choose c s] handles state changes when an ai with color [c] moves. *)
val choose : color -> state -> Command.command
