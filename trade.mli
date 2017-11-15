open Tile

open Player

(* Trade module handles tradings of the game *)

(* port is the type of trading ports of the game *)
type port

(* [get_exchange_rate reso] returns the exchange rate of the given [reso] *)
val get_exchange_rate : resource -> int*int
